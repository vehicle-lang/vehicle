{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Compile.Type
  ( TypingError(..)
  , typeCheck
  ) where

import Prelude hiding (pi)
import Control.Monad (when)
import Control.Monad.Except (MonadError(..), ExceptT)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.State (MonadState, evalStateT)
import Data.Foldable (foldrM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List.NonEmpty (NonEmpty(..))
import Prettyprinter ((<+>), Pretty(..))

import Vehicle.Core.AST
import Vehicle.Core.Compile.DSL
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Unify
import Vehicle.Core.Compile.Type.Meta
import Vehicle.Core.Compile.Type.TypeClass
import Vehicle.Prelude

typeCheck :: UncheckedProg -> ExceptT TypingError Logger CheckedProg
typeCheck prog = do
  let prog1 = runAll prog
  let prog2 = runReaderT prog1 emptyVariableCtx
  prog3 <- evalStateT prog2 emptyMetaCtx
  logDebug $ "Program:" <+> pretty prog3
  return prog3

runAll :: TCM m => UncheckedProg -> m CheckedProg
runAll prog = do
  logDebug $ pretty prog
  prog2 <- inferProg prog
  metaSubstitution <- solveMetas
  return $ substMetas metaSubstitution prog2

solveMetas :: TCM m => m MetaSubstitution
solveMetas = do
  unificationProgress <- solveUnificationConstraints
  tcResolutionProgress <- solveTypeClassConstraints

  let progress = unificationProgress || tcResolutionProgress
  unsolvedUnificationConstraints <- getUnificationConstraints
  unsolvedTypeClassConstraints <- getTypeClassConstraints

  case (unsolvedUnificationConstraints, unsolvedTypeClassConstraints, progress) of
    ([], [], _)        -> getMetaSubstitution
    (_, _, True)       -> solveMetas
    (c : cs, _, False) -> throwError $ UnsolvedUnificationConstraints (c :| cs)
    (_, c : cs, False) -> throwError $ UnsolvedTypeClassConstraints (c :| cs)

--------------------------------------------------------------------------------
-- Contexts

-- | The type-checking monad
type TCM m =
  ( MonadError  TypingError m
  , MonadState  MetaCtx     m
  , MonadReader VariableCtx m
  , MonadLogger             m
  )

data VariableCtx = VariableCtx
  { boundCtx :: BoundCtx
  , declCtx  :: DeclCtx
  }

emptyVariableCtx :: VariableCtx
emptyVariableCtx = VariableCtx mempty mempty

-- | The declarations that are currently in scope, indexed into via their names.
type DeclCtx = Map Identifier CheckedExpr

instance Pretty DeclCtx where
  pretty = pretty . show

getDeclCtx :: TCM m => m DeclCtx
getDeclCtx = asks declCtx

addToDeclCtx :: TCM m => Identifier -> CheckedExpr -> m a -> m a
addToDeclCtx n e = local add
  where
    add :: VariableCtx -> VariableCtx
    add VariableCtx{..} = VariableCtx{declCtx = Map.insert n e declCtx, ..}

getBoundCtx :: TCM m => m BoundCtx
getBoundCtx = asks boundCtx

addToBoundCtx :: TCM m => Name -> CheckedExpr -> m a -> m a
addToBoundCtx n e = local add
  where
    add :: VariableCtx -> VariableCtx
    add VariableCtx{..} = VariableCtx{boundCtx = (n, e) : boundCtx, ..}

--------------------------------------------------------------------------------
-- Debug functions

showCheckEntry :: TCM m => CheckedExpr -> UncheckedExpr -> m (CheckedExpr, UncheckedExpr)
showCheckEntry t e = do
  logDebug ("check-entry" <+> pretty e <+> "<-" <+> pretty t)
  incrCallDepth
  return (t,e)

showCheckExit :: TCM m => m CheckedExpr -> m CheckedExpr
showCheckExit m = do
  e <- m
  decrCallDepth
  logDebug ("check-exit " <+> pretty e)
  return e

showInferEntry :: TCM m => UncheckedExpr -> m UncheckedExpr
showInferEntry e = do
  logDebug ("infer-entry" <+> pretty e)
  incrCallDepth
  return e

showInferExit :: TCM m => m (CheckedExpr, CheckedExpr) -> m (CheckedExpr, CheckedExpr)
showInferExit m = do
  (e, t) <- m
  decrCallDepth
  logDebug ("infer-exit " <+> pretty e <+> "->" <+> pretty t)
  return (e,t)

showInsertArg :: TCM m => Visibility -> CheckedExpr -> m CheckedExpr
showInsertArg v t = do
  logDebug ("insert-arg" <+> pretty v <+> pretty t)
  return t

-------------------------------------------------------------------------------
-- Utility functions

assertIsType :: TCM m => Provenance -> CheckedExpr -> m ()
assertIsType _ (Type _) = return ()
-- TODO: add a new TypingError 'ExpectedAType'
assertIsType p e        = throwError $ Mismatch p e (Type 0)

unify :: TCM m => Provenance -> CheckedExpr -> CheckedExpr -> m CheckedExpr
unify p e1 e2 = do
  ctx <- getBoundCtx
  addUnificationConstraint $ makeConstraint p ctx e1 e2
  -- TODO calculate the most general unifier
  return e1

freshMeta :: TCM m
          => Provenance
          -> m (Meta, CheckedExpr)
freshMeta p = do
  ctx <- getBoundCtx
  freshMetaWith ctx p

-- Generate new metavariables for any implicit/constraint arguments
insertArgs :: TCM m
           => Provenance
           -> CheckedExpr -- The function
           -> CheckedExpr -- The supposed type
           -> m (CheckedExpr, CheckedExpr)
insertArgs p fun' (Pi _ (Binder _ vArg _ tArg') tRes')
  | vArg /= Explicit = do
  tArg'' <- showInsertArg vArg tArg'

  ctx <- getBoundCtx
  (meta, metaArg) <- freshMetaWith ctx p

  -- Substitute meta-variable in tRes
  let tResSubst' = metaArg `substInto` tRes'

  -- TODO Wen is worried about interactions between the Pi abstractions over
  -- the context and the type-class search later on.

  -- Check if the required argument is a type-class
  when (vArg == Constraint) $
    addTypeClassConstraint (meta `Has` tArg'')

  insertArgs p (App mempty fun' (Arg mempty vArg metaArg)) tResSubst'
insertArgs _ fun' typ' =
  return (fun', typ')

--------------------------------------------------------------------------------
-- Type-checking of expressions

check :: TCM m
      => CheckedExpr   -- Type we're checking against
      -> UncheckedExpr -- Expression being type-checked
      -> m CheckedExpr -- Updated expression
check expectedType expr = showCheckExit $ do
  r <- showCheckEntry expectedType expr
  case r of
    (Pi _ (Binder _ vFun _ tBound2) tRes, Lam p (Binder pBound vBound nBound tBound1) body)
      | vFun == vBound -> do
        tBound' <- check tBound2 tBound1

        addToBoundCtx nBound tBound' $ do
          body' <- check tRes body
          return $ Lam p (Binder pBound Implicit nBound tBound') body'

    (Pi _ (Binder _ Implicit name tBound2) tRes, e) ->
      -- Add the implict argument to the context
      addToBoundCtx Machine tBound2 $ do
        -- Check if the type matches the expected result type.
        e' <- check tRes (liftDBIndices 1 e)

        -- Create a new binder mirroring the implicit Pi binder expected
        let newBinder = Binder mempty Implicit name tBound2

        -- Prepend a new lambda to the expression with the implicit binder
        return $ Lam mempty newBinder e'

    (_, Lam p (Binder _ vBound _ _) _) -> do
          let expected = fromDSL $ unnamedPi vBound (tHole "a") (const (tHole "b"))
          throwError $ Mismatch p expectedType expected

    (_, Hole p _name) -> do
      -- Replace the hole with meta-variable of the expected type.
      -- NOTE, different uses of the same hole name will be interpreted as different meta-variables.
      (_, meta) <- freshMeta p
      unify p meta expectedType

    (_, e@(Type _))          -> viaInfer mempty expectedType e
    (_, e@(Meta _ _))        -> viaInfer mempty expectedType e
    (_, e@(App     p _ _))   -> viaInfer p      expectedType e
    (_, e@(Pi      p _ _))   -> viaInfer p      expectedType e
    (_, e@(Builtin p _))     -> viaInfer p      expectedType e
    (_, e@(Var     p _))     -> viaInfer p      expectedType e
    (_, e@(Let     p _ _ _)) -> viaInfer p      expectedType e
    (_, e@(Literal p _))     -> viaInfer p      expectedType e
    (_, e@(Seq     p _))     -> viaInfer p      expectedType e
    (_, e@(Ann     p _ _))   -> viaInfer p      expectedType e
    (_, PrimDict{})          -> developerError "PrimDict should never be type-checked"

inferApp :: TCM m
         => Provenance
         -> UncheckedArg
         -> CheckedExpr
         -> CheckedExpr
         -> m (CheckedExpr, CheckedExpr)
inferApp p (Arg pArg vArg eArg) fun' = \case
  -- Check if it's a Pi type which is expecting the right visibility
  Pi _ (Binder _ vFun _ tArg') tRes'
    | vArg == vFun -> do
      -- Check the type of the argument.
      eArg' <- check tArg' eArg

      -- Substitute argument in tRes
      let tResSubst' = eArg' `substInto` tRes'

      -- Return the appropriately annotated type with its inferred kind.
      return (App p fun' (Arg pArg vArg eArg') , tResSubst')

  tFun' -> do
    let expected = fromDSL $ unnamedPi vArg (tHole "a") (const $ tHole "b")
    throwError $ Mismatch p tFun' expected

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
infer :: TCM m
      => UncheckedExpr
      -> m (CheckedExpr, CheckedExpr)
infer e = showInferExit $ do
  r <- showInferEntry e
  case r of
    Type l ->
      return (Type l , Type (l + 1))

    Meta _ m -> developerError $ "Trying to infer the type of a meta-variable" <+> pretty m

    Hole ann s ->
      throwError $ UnresolvedHole (prov ann) s

    Ann ann expr t   -> do
      (t', _) <- infer t
      expr' <- check t' expr
      return (Ann ann expr' t' , t')

    App p fun arg@(Arg _ vArg _) -> do
      -- Infer the type of the function.
      (fun', tFun') <- infer fun
      -- if this is an explicit argument, then insert implicits before proceeding
      (fun'', tFun'') <- if vArg == Explicit
        then insertArgs p fun' tFun'
        else return (fun', tFun')
      inferApp p arg fun'' tFun''

    Pi p (Binder pBound v name arg) res -> do
      (arg', tArg') <- infer arg

      addToBoundCtx name arg' $ do
        (res', tRes') <- infer res
        let t' = tArg' `tMax` tRes'
        return (Pi p (Binder pBound v name arg') res' , t')

    Var p (Bound i) -> do
      -- Lookup the type of the variable in the context.
      ctx <- getBoundCtx
      case ctx !!? i of
        Just (_, t') -> do
          let t'' = liftDBIndices (i+1) t'
          return (Var p (Bound i), t'')
        Nothing      -> developerError $
          "Index" <+> pretty i <+> "out of bounds when looking up variable in context" <+> pretty ctx <+> "at" <+> pretty p

    Var p (Free ident) -> do
      -- Lookup the type of the declaration variable in the context.
      ctx <- getDeclCtx
      case Map.lookup ident ctx of
        Just t' -> return (Var p (Free ident), t')
        -- This should have been caught during scope checking
        Nothing -> developerError $
          "Declaration'" <+> pretty ident <+> "'not found when looking up variable in context" <+> pretty ctx <+> "at" <+> pretty p

    Let p bound (Binder pBound v name tBound)  body -> do
      -- Infer the type of the let arg from the annotation on the binder
      (tBound', _) <- infer tBound

      -- Check the bound expression actually has that type
      bound' <- check tBound' bound

      -- Update the context with the bound variable
      addToBoundCtx name tBound' $ do
        -- Infer the type of the body
        (body' , tBody') <- infer body
        return (Let p bound' (Binder pBound v name tBound') body' , tBody')

    Lam p (Binder pBound v name tBound) body -> do
      -- Infer the type of the bound variable from the binder
      (tBound', _) <- infer tBound

      -- Update the context with the bound variable
      addToBoundCtx name tBound' $ do
        (body' , tBody') <- infer body
        let t' = Pi p (Binder pBound v name tBound') tBody'
        return (Lam p (Binder pBound v name tBound') body' , t')

    Literal p l -> do
      let t' = typeOfLiteral p l
      (expr', type') <- insertArgs p (Literal p l) t'
      return (expr', type')

    Builtin p op -> do
      let t' = typeOfBuiltin p op
      return (Builtin p op, t')

    Seq p es -> do
      (es', ts') <- unzip <$> traverse infer es

      -- Generate a fresh meta variable for the type of elements in the list, e.g. Int
      (_, tElem') <- freshMeta p
      -- Generate a meta-variable for the applied container type, e.g. List Int
      (_, tCont') <- freshMeta p

      -- Unify the types of all the elements in the sequence
      _ <- foldrM (unify p) tElem' ts'

      -- Enforce that the applied container type must be a container
      -- Generate a fresh meta-variable for the container function, e.g. List
      (meta, _) <- freshMeta p
      addTypeClassConstraint (meta `Has` isContainer' p tCont' tElem')

      return (Seq p es' , tCont')

    PrimDict{} -> developerError "PrimDict should never be type-checked"

-- TODO: unify DeclNetw and DeclData
inferDecls :: TCM m => [UncheckedDecl] -> m [CheckedDecl]
inferDecls [] = return []
inferDecls (DeclNetw p ident t : decls) = do
    (t', ty') <- infer t
    assertIsType p ty'

    decls' <- addToDeclCtx (deProv ident) t' $ do
      inferDecls decls
    return $ DeclNetw p ident t' : decls'
inferDecls (DeclData p ident t : decls) = do
    (t', ty') <- infer t
    assertIsType p ty'

    decls' <- addToDeclCtx (deProv ident) t' $ do
      inferDecls decls
    return $ DeclData p ident t' : decls'
inferDecls (DefFun p ident t body : decls) = do
    (t', ty') <- infer t
    assertIsType p ty'

    body' <- check t' body

    decls' <- addToDeclCtx (deProv ident) t' $ do
      inferDecls decls

    return $ DefFun p ident t' body' : decls'

inferProg :: TCM m => UncheckedProg -> m CheckedProg
inferProg (Main ds) = do
  logDebug "Beginning initial type-checking pass"
  result <- Main <$> inferDecls ds
  logDebug "Ending initial type-checking pass\n"
  return result

viaInfer :: TCM m => Provenance -> CheckedExpr -> UncheckedExpr -> m CheckedExpr
viaInfer p expectedType e = do
  -- TODO may need to change the term when unifying to insert a type application.
  (e', actualType) <- infer e
  (e'', actualType') <- insertArgs p e' actualType
  _t <- unify p expectedType actualType'
  return e''

--------------------------------------------------------------------------------
-- Typing of literals and builtins

-- | Return the type of the provided literal,
typeOfLiteral :: Provenance -> Literal -> CheckedExpr
typeOfLiteral p l = fromDSL $ case l of
  LNat  _ -> forall type0 $ \t -> isNatural  p t ~~~> t
  LInt  _ -> forall type0 $ \t -> isIntegral p t ~~~> t
  LRat  _ -> forall type0 $ \t -> isReal     p t ~~~> t
  LBool _ -> forall type0 $ \t -> isTruth    p t ~~~> t

-- | Return the type of the provided builtin.
typeOfBuiltin :: Provenance -> Builtin -> CheckedExpr
typeOfBuiltin p b = fromDSL $ case b of
  Bool            -> type0
  Prop            -> type0
  Nat             -> type0
  Int             -> type0
  Real            -> type0
  List            -> type0 ~> type0
  Tensor          -> type0 ~> tList tNat ~> type0

  HasEq           -> type0 ~> type0 ~> type0
  HasOrd          -> type0 ~> type0 ~> type0
  IsTruth         -> type0 ~> type0
  IsNatural       -> type0 ~> type0
  IsIntegral      -> type0 ~> type0
  IsRational      -> type0 ~> type0
  IsReal          -> type0 ~> type0
  IsContainer     -> type0 ~> type0
  IsQuantifiable  -> type0 ~> type0 ~> type0

  If   -> typeOfIf
  Cons -> typeOfCons

  Impl -> typeOfBoolOp2 p
  And  -> typeOfBoolOp2 p
  Or   -> typeOfBoolOp2 p
  Not  -> typeOfBoolOp1 p

  Eq   -> typeOfEqualityOp p
  Neq  -> typeOfEqualityOp p

  Le   -> typeOfComparisonOp p
  Lt   -> typeOfComparisonOp p
  Ge   -> typeOfComparisonOp p
  Gt   -> typeOfComparisonOp p

  Add  -> typeOfNumOp2 (isNatural  p)
  Sub  -> typeOfNumOp2 (isIntegral p)
  Mul  -> typeOfNumOp2 (isNatural  p)
  Div  -> typeOfNumOp2 (isRational p)
  Neg  -> typeOfNumOp1 (isIntegral p)

  At   -> typeOfAtOp p
  Map  -> typeOfMapOp
  Fold -> typeOfFoldOp p

  Quant _ -> typeOfQuantifierOp

typeOfIf :: DSLExpr
typeOfIf =
  forall type0 $ \t ->
    tProp ~> t ~> t

typeOfCons :: DSLExpr
typeOfCons =
  forall type0 $ \t ->
    t ~> tList t ~> tList t

typeOfEqualityOp :: Provenance -> DSLExpr
typeOfEqualityOp p =
  forall type0 $ \t ->
    forall type0 $ \r ->
      hasEq p t r ~~~> t ~> t ~> r

typeOfComparisonOp :: Provenance -> DSLExpr
typeOfComparisonOp p =
  forall type0 $ \t ->
    forall type0 $ \r ->
      hasOrd p t r ~~~> t ~> t ~> r

typeOfBoolOp2 :: Provenance -> DSLExpr
typeOfBoolOp2 p =
  forall type0 $ \t ->
    isTruth p t ~~~> t ~> t ~> t

typeOfBoolOp1 :: Provenance -> DSLExpr
typeOfBoolOp1 p =
  forall type0 $ \t ->
    isTruth p t ~~~> t ~> t

typeOfNumOp2 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp2 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~~> t ~> t ~> t

typeOfNumOp1 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp1 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~~> t ~> t

typeOfQuantifierOp :: DSLExpr
typeOfQuantifierOp =
  forall type0 $ \t ->
    (t ~> tProp) ~> tProp

typeOfAtOp :: Provenance -> DSLExpr
typeOfAtOp p =
  forall type0 $ \tCont ->
    forall type0 $ \tElem ->
      isContainer p tCont tElem ~~~> tCont ~> tNat ~> tElem

-- TODO generalise these to tensors etc.
typeOfMapOp :: DSLExpr
typeOfMapOp =
  forall type0 $ \tFrom ->
    forall type0 $ \tTo ->
      (tFrom ~> tTo) ~> tList tFrom ~> tList tTo

typeOfFoldOp :: Provenance -> DSLExpr
typeOfFoldOp p =
  forall type0 $ \tCont ->
    forall type0 $ \tElem ->
      forall type0 $ \tRes ->
        isContainer p tCont tElem ~~~> (tElem ~> tRes ~> tRes) ~> tRes ~> tCont ~> tRes