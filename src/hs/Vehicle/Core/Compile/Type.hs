{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Compile.Type
  ( TypingError(..)
  , runTypeChecking
  ) where

import Prelude hiding (pi)
import Control.Monad (when)
import Control.Monad.Except (MonadError(..), Except, withExcept)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.State (MonadState(..), StateT(..), modify, runStateT)
import Debug.Trace (trace)
import Data.Text (Text)
import Data.Foldable (toList, foldrM)
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Functor ((<&>))
import Data.Bifunctor (first, second)
import Prettyprinter ((<+>), Pretty(..), line)

import Vehicle.Core.AST
import Vehicle.Core.Compile.DSL
import Vehicle.Core.Compile.Unify
import Vehicle.Core.Compile.Metas
import Vehicle.Core.Print.Core (showCore)
import Vehicle.Prelude

-- TODO:
--
--  - Add support for unification with meta-variables.

runTypeChecking :: UncheckedProg -> Except TypingError CheckedProg
runTypeChecking prog = do
  let prog1 = inferProg prog
  let prog3 = runReaderT prog1 emptyVariableCtx
  let prog4 = runStateT  prog3 (emptyMetaCtx, 0)
  (checkedProg, (metaCtx, _)) <- prog4
  trace (layoutAsString $ pretty metaCtx) (return ())
  let constraints = unificationConstraints metaCtx
  (unsolvedConstraints, metaSubst) <- withExcept UnificationError (solve (constraints, mempty))
  trace (layoutAsString $ prettyMetaSubst metaSubst) (return ())
  case unsolvedConstraints of
    []     -> return $ substMetas metaSubst checkedProg
    c : cs -> throwError $ UnsolvedConstraints (c :| cs)

-- To solve / synthesise typeClassConstraints :
--  1. Normalise each one in the current metactxt
--  2. Insert lambdas for each Pi type
--  3. Match against (a) the list of built-in instances; then (b) against elements in the context
--  4. If the constraint is a Z3 one, then ask Z3 to solve it (after normalisation)

--------------------------------------------------------------------------------
-- Errors

-- | Errors thrown during type checking
data TypingError
  = UnresolvedHole
    Provenance              -- The location of the hole
    Symbol                  -- The name of the hole
  | Mismatch
    Provenance              -- The location of the mismatch.
    CheckedExpr             -- The possible inferred types.
    DSLExpr             -- The expected type.
  | UnsupportedOperation
    Provenance              -- The location of the unsupported operation.
    Text                    -- A description of the unsupported operation.
  | UnsolvedConstraints
    (NonEmpty UnificationConstraint) -- The constraints that could not be solved
  | UnificationError
    UnificationError

instance MeaningfulError TypingError where
  details (Mismatch p candidate expected) = UError $ UserError
    { provenance = p
    , problem    = "expected something of type" <+> pretty (fromDSL expected) <+>
                   "but inferred type" <+> pretty candidate
    , fix        = "unknown"
    }

  details (UnsupportedOperation p t) = UError $ UserError
    { provenance = p
    , problem    = "type-checking of" <+> squotes t <+> "not currently supported"
    , fix        = "unknown"
    }

  details (UnresolvedHole p name) = UError $ UserError
    { provenance = p
    , problem    = "the type of" <+> squotes name <+> "could not be resolved"
    , fix        = "unknown"
    }

  details (UnsolvedConstraints cs) = let firstConstraint = NonEmpty.head cs in
    UError $ UserError
    { provenance = prov firstConstraint
    , problem    = "unsolved constraint " <+> pretty firstConstraint
    , fix        = "unknown"
    }

  details (UnificationError e) = details e

--------------------------------------------------------------------------------
-- Contexts

-- Purely for debugging purposes
type CallDepth = Int

-- | The type-checking monad
type TCM m =
  ( MonadError  TypingError          m
  , MonadState  (MetaCtx, CallDepth) m
  , MonadReader VariableCtx          m
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

-- | The names and types of the expression variables that are in currently in scope,
-- indexed into via De Bruijn expressions.
type BoundCtx = [(Name, CheckedExpr)]

getBoundCtx :: TCM m => m BoundCtx
getBoundCtx = asks boundCtx

addToBoundCtx :: TCM m => Name -> CheckedExpr -> m a -> m a
addToBoundCtx n e = local add
  where
    add :: VariableCtx -> VariableCtx
    add VariableCtx{..} = VariableCtx{boundCtx = (n, e) : boundCtx, ..}

-- TODO ask Bob whether we need to store metaVariableTypes explicitly in the ctx
-- Will we get in trouble storing the type on the meta?

-- | The meta-variables and constraints relating the variables currently in scope.
data MetaCtx  = MetaCtx
  { nextMeta               :: Meta
  , metaVariableTypes      :: IntMap CheckedExpr
  , unificationConstraints :: [UnificationConstraint]
  , typeClassConstraints   :: [TypeClassConstraint]
  }

instance Pretty MetaCtx where
  pretty MetaCtx{..} = "{" <> line <>
    "nextMeta" <+> "=" <+> pretty nextMeta <> line <>
    "unificationConstraints" <+> "=" <+> pretty unificationConstraints <> line <>
    "typeClassConstraints" <+> "=" <+> pretty typeClassConstraints <> line <>
    "}"

emptyMetaCtx :: MetaCtx
emptyMetaCtx = MetaCtx
  { nextMeta               = 0
  , metaVariableTypes      = mempty
  , unificationConstraints = mempty
  , typeClassConstraints   = mempty
  }

getMetaCtx :: TCM m => m MetaCtx
getMetaCtx = get <&> fst

modifyMetaCtx :: TCM m => (MetaCtx -> MetaCtx) -> m ()
modifyMetaCtx f = modify (first f)

putMetaCtx :: TCM m => MetaCtx -> m ()
putMetaCtx ctx = modifyMetaCtx (const ctx)

modifyCallDepth :: TCM m => (CallDepth -> CallDepth) -> m CallDepth
modifyCallDepth f = do
  (c, d) <- get
  put (c, f d)
  return d

addUnificationConstraints :: TCM m => [UnificationConstraint] -> m ()
addUnificationConstraints cs = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { unificationConstraints = cs <> unificationConstraints, ..}

unify :: TCM m => Provenance -> CheckedExpr -> CheckedExpr -> m CheckedExpr
unify p e1 e2 = do
  ctx <- getBoundCtx
  -- TODO calculate the context (currently breaks the printing of unification constraints)
  addUnificationConstraints [makeConstraint p ctx e1 e2]
  -- TODO calculate the most general unifier
  return e1

addTypeClassConstraints :: TCM m => [TypeClassConstraint] -> m ()
addTypeClassConstraints ts = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { typeClassConstraints = ts <> typeClassConstraints, ..}

{-
getMetaType :: Meta -> TCM CheckedExpr
getMetaType i = do
  ctx <- getMetaCtx;
  case IntMap.lookup i (metaVariableTypes ctx) of
    Just typ -> return typ
    Nothing  -> developerError ("Meta variable ?" <> pretty i <+> "not found in context")
-}
freshMetaName :: TCM m => m Meta
freshMetaName = do
  MetaCtx {..} <- getMetaCtx;
  putMetaCtx $ MetaCtx { nextMeta = succ nextMeta , ..}
  return nextMeta

-- TODO unify these functions in a pleasing way

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMeta :: TCM m => Provenance -> CheckedExpr -> m (Meta, CheckedExpr, CheckedExpr)
freshMeta p resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx <- getBoundCtx
  let metaType =
        foldr (\(name, t) resTy ->
                  Pi (RecAnn (piType (getType t) (getType resTy)) mempty)
                     (Binder mempty Explicit name t)
                     resTy)
          resultType
          (reverse boundCtx)

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv =
        reverse [ Var (RecAnn varType mempty) (Bound varIndex)
                | (varIndex , (_ , varType)) <- zip [0..] (toList boundCtx) ]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' cApp (Meta (RecAnn metaType p) metaName) boundEnv
  return (metaName, meta, metaType)


--------------------------------------------------------------------------------
-- Type-checking of expressions

showCheckEntry :: TCM m => CheckedExpr -> UncheckedExpr -> m (CheckedExpr, UncheckedExpr)
showCheckEntry t e = do
  d <- modifyCallDepth (+1)
  return $ trace (duplicate " " d <> "check-entry " <> showCore e <> " <= " <> showCore t) (t,e)

showCheckExit :: TCM m => m CheckedExpr -> m CheckedExpr
showCheckExit m = do
  e <- m
  d <- modifyCallDepth (\x->x-1)
  trace (duplicate " " (d-1) <> "check-exit  " <> showCore e) (return e)

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
          return $ Lam (RecAnn expectedType p) (Binder pBound Implicit nBound tBound') body'

    (Pi _ (Binder _ Implicit name tBound2) tRes, e) ->
      -- Add the implict argument to the context
      addToBoundCtx Machine tBound2 $ do
        -- Check if the type matches the expected result type.
        e' <- check tRes (liftDBIndices 1 e)

        -- Create a new binder mirroring the implicit Pi binder expected
        let newBinder = Binder mempty Implicit name tBound2

        -- Prepend a new lambda to the expression with the implicit binder
        return $ Lam (RecAnn expectedType mempty) newBinder e'

    (_, Lam p (Binder _ vBound _ _) _) -> do
          let expected = unnamedPi vBound (tHole "a") (const (tHole "b"))
          throwError $ Mismatch p expectedType expected

    (_, Hole p _name) -> do
      -- Replace the hole with meta-variable of the expected type.
      -- NOTE, different uses of the same hole name will be interpreted as different meta-variables.
      (_, meta, _) <- freshMeta p expectedType
      return meta

    (_, e@(Type _))          -> viaInfer mempty expectedType e
    (_, e@Constraint)        -> viaInfer mempty expectedType e
    (_, e@(Meta _ _))        -> viaInfer mempty expectedType e
    (_, e@(App     p _ _))   -> viaInfer p      expectedType e
    (_, e@(Pi      p _ _))   -> viaInfer p      expectedType e
    (_, e@(Builtin p _))     -> viaInfer p      expectedType e
    (_, e@(Var     p _))     -> viaInfer p      expectedType e
    (_, e@(Let     p _ _ _)) -> viaInfer p      expectedType e
    (_, e@(Literal p _))     -> viaInfer p      expectedType e
    (_, e@(Seq     p _))     -> viaInfer p      expectedType e
    (_, e@(Ann     p _ _))   -> viaInfer p      expectedType e

-- Generate new metavariables for any implicit arguments
insertImplicits :: TCM m => Provenance -> CheckedExpr -> CheckedExpr -> m (CheckedExpr, CheckedExpr)
insertImplicits p fun' (Pi _ (Binder _ Implicit _ tArg') tRes') = do
  (meta, metaArg, metaType) <- freshMeta p tArg'

  -- Substitute meta-variable in tRes
  let tResSubst' = metaArg `substInto` tRes'

  -- TODO Wen is worried about interactions between the Pi abstractions over
  -- the context and the type-class search later on.

  -- Check if the implicit argument is a type-class
  when (isConstraint tArg') $
    addTypeClassConstraints [meta `Has` metaType]

  insertImplicits p (App (RecAnn tResSubst' mempty) fun' (Arg mempty Implicit metaArg)) tResSubst'
insertImplicits _ fun' typ' =
  return (fun', typ')


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
      return (App (RecAnn tResSubst' p) fun' (Arg pArg vArg eArg') , tResSubst')

  tFun' -> do
    let expected = unnamedPi vArg (tHole "a") (const $ tHole "b")
    throwError $ Mismatch p tFun' expected


showInferEntry :: TCM m => UncheckedExpr -> m UncheckedExpr
showInferEntry e = do
  d <- modifyCallDepth (+1)
  return $ trace (duplicate " " d <> "infer-entry " <> showCore e) e

showInferExit :: TCM m => m (CheckedExpr, CheckedExpr) -> m (CheckedExpr, CheckedExpr)
showInferExit m = do
  (e, t) <- m
  d <- modifyCallDepth (\x -> x-1)
  trace (duplicate " " (d-1) <> "infer-exit  " <> showCore e <> " => " <> showCore t) (return (e,t))

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

    Constraint ->
      return (Constraint , Type 1)

    Meta _ m -> developerError $ "Trying to infer the type of a meta-variable" <+> pretty m

    Hole ann s ->
      throwError $ UnresolvedHole (prov ann) s

    Ann ann expr t   -> do
      (t', _) <- infer t
      expr' <- check t' expr
      let ann' = RecAnn t' ann
      return (Ann ann' expr' t' , t')

    App p fun arg@(Arg _ vArg _) -> do
      -- Infer the type of the function.
      (fun', tFun') <- infer fun
      -- if this is an explicit argument, then insert implicits before proceeding
      (fun', tFun') <- if vArg == Explicit then insertImplicits p fun' tFun' else return (fun', tFun')
      inferApp p arg fun' tFun'

    Pi p (Binder pBound vis name arg) res -> do
      (arg', tArg') <- infer arg

      addToBoundCtx name arg' $ do
        (res', tRes') <- infer res
        let t' = tArg' `tMax` tRes'
        return (Pi (RecAnn t' p) (Binder pBound vis name arg') res' , t')

    Builtin p op -> do
      let t' = typeOfBuiltin p op
      return (Builtin (RecAnn t' p) op, t')

    Var p (Bound i) -> do
      -- Lookup the type of the variable in the context.
      ctx <- getBoundCtx
      case ctx !!? i of
        Just (_, t') -> do
          let t'' = liftDBIndices (i+1) t'
          return (Var (RecAnn t'' p) (Bound i), t'')
        Nothing      -> developerError $
          "Index" <+> pretty i <+> "out of bounds when looking up variable in context" <+> pretty ctx <+> "at" <+> pretty p

    Var p (Free ident) -> do
      -- Lookup the type of the declaration variable in the context.
      ctx <- getDeclCtx
      case Map.lookup ident ctx of
        Just t' -> return (Var (RecAnn t' p) (Free ident), t')
        -- This should have been caught during scope checking
        Nothing -> developerError $
          "Declaration'" <+> pretty ident <+> "'not found when looking up variable in context" <+> pretty ctx <+> "at" <+> pretty p

    Let p bound (Binder pBound vis name tBound)  body -> do
      -- Infer the type of the let arg from the annotation on the binder
      (tBound', _) <- infer tBound

      -- Check the bound expression actually has that type
      bound' <- check tBound' bound

      -- Update the context with the bound variable
      addToBoundCtx name tBound' $ do
        -- Infer the type of the body
        (body' , tBody') <- infer body
        return (Let (RecAnn tBody' p) bound' (Binder pBound vis name tBound') body' , tBody')

    Lam p (Binder pBound vis name tBound) body -> do
      -- Infer the type of the bound variable from the binder
      (tBound', _) <- infer tBound

      -- Update the context with the bound variable
      addToBoundCtx name tBound' $ do
        (body' , tBody') <- infer body
        let t' = Pi (RecAnn (piType tBound' tBody') p) (Binder pBound vis name tBound') tBody'
        return (Lam (RecAnn t' p) (Binder pBound vis name tBound') body' , t')

    Literal p l -> do
      let t' = typeOfLiteral p l
      return (Literal (RecAnn t' p) l, t')

    Seq p es -> do
      (es', ts') <- unzip <$> traverse infer es

      -- Generate a fresh meta variable for the case where the list is empty
      (_, tElem, _) <- freshMeta p Type0

      -- Unify the types of all the elements in the list
      tElem' <- foldrM (unify p) tElem ts'

      -- Generate a meta-variable for the type of the container
      (meta, tCont', _) <- freshMeta p Type0
      -- addTypeClassConstraints [meta `Has` unembed (isContainer p (embed tCont') (embed tElem'))]

      return (Seq (RecAnn tCont' p) es' , tCont')

assertIsType :: TCM m => Provenance -> CheckedExpr -> m ()
assertIsType _ (Type _) = return ()
assertIsType p e        = throwError $ Mismatch p e type0 -- TODO: add a new TypingError 'ExpectedAType'

-- TODO: unify DeclNetw and DeclData
checkDecls :: TCM m => [UncheckedDecl] -> m [CheckedDecl]
checkDecls [] = return []
checkDecls (DeclNetw p ident t : decls) = do
    (t', ty') <- infer t
    assertIsType p ty'

    decls' <- addToDeclCtx (deProv ident) t' $ do
      checkDecls decls
    return $ DeclNetw p ident t' : decls'
checkDecls (DeclData p ident t : decls) = do
    (t', ty') <- infer t
    assertIsType p ty'

    decls' <- addToDeclCtx (deProv ident) t' $ do
      checkDecls decls
    return $ DeclData p ident t' : decls'
checkDecls (DefFun p ident t body : decls) = do
    let _ = trace ("check-fun-entry" <> layoutAsString (pretty ident))
    (t', ty') <- infer t
    assertIsType p ty'

    body' <- check t' body

    decls' <- addToDeclCtx (deProv ident) t' $ do
      checkDecls decls

    let _ = trace ("check-fun-exit" <> layoutAsString (pretty ident))
    return $ DefFun p ident t' body' : decls'

inferProg :: TCM m => UncheckedProg -> m CheckedProg
inferProg (Main ds) = Main <$> checkDecls ds

viaInfer :: TCM m => Provenance -> CheckedExpr -> UncheckedExpr -> m CheckedExpr
viaInfer p expectedType e = do
  -- TODO may need to change the term when unifying to insert a type application.
  (e', actualType) <- infer e
  (e'', actualType') <- insertImplicits p e' actualType
  _t' <- unify p expectedType actualType'
  return e''

--------------------------------------------------------------------------------
-- Typing of literals

typeOfLiteral :: Provenance -> Literal -> CheckedExpr
typeOfLiteral p l = fromDSL $ case l of
  LNat  _ -> forall type0 $ \t -> isNatural  p t ~~> t
  LInt  _ -> forall type0 $ \t -> isIntegral p t ~~> t
  LRat  _ -> forall type0 $ \t -> isReal     p t ~~> t
  LBool _ -> forall type0 $ \t -> isTruth    p t ~~> t

--------------------------------------------------------------------------------
-- Typing of builtins

-- |Return the kind for builtin exprs.
typeOfBuiltin :: Provenance -> Builtin -> CheckedExpr
typeOfBuiltin p b = fromDSL $ case b of
  Bool            -> type0
  Prop            -> type0
  Nat             -> type0
  Int             -> type0
  Real            -> type0
  List            -> type0 ~> type0
  Tensor          -> type0 ~> tList tNat ~> type0

  HasEq           -> type0 ~> type0 ~> constraint
  HasOrd          -> type0 ~> type0 ~> constraint
  IsTruth         -> type0 ~> constraint
  IsNatural       -> type0 ~> constraint
  IsIntegral      -> type0 ~> constraint
  IsRational      -> type0 ~> constraint
  IsReal          -> type0 ~> constraint
  IsContainer     -> type0 ~> constraint
  IsQuantifiable  -> type0 ~> type0 ~> constraint

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
  Map  -> typeOfMapOp p
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
      hasEq p t r ~~> t ~> t ~> r

typeOfComparisonOp :: Provenance -> DSLExpr
typeOfComparisonOp p =
  forall type0 $ \t ->
    forall type0 $ \r ->
      hasOrd p t r ~~> t ~> t ~> r

typeOfBoolOp2 :: Provenance -> DSLExpr
typeOfBoolOp2 p =
  forall type0 $ \t ->
    isTruth p t ~~> t ~> t ~> t

typeOfBoolOp1 :: Provenance -> DSLExpr
typeOfBoolOp1 p =
  forall type0 $ \t ->
    isTruth p t ~~> t ~> t

typeOfNumOp2 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp2 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~> t ~> t ~> t

typeOfNumOp1 :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfNumOp1 numConstraint =
  forall type0 $ \t ->
    numConstraint t ~~> t ~> t

typeOfQuantifierOp :: DSLExpr
typeOfQuantifierOp =
  forall type0 $ \t ->
    (t ~> tProp) ~> tProp

typeOfAtOp :: Provenance -> DSLExpr
typeOfAtOp p =
  forall type0 $ \tCont ->
    forall type0 $ \tElem ->
      isContainer p tCont tElem ~~> tCont ~> tNat ~> tElem

-- TODO generalise these to tensors etc.
typeOfMapOp :: Provenance -> DSLExpr
typeOfMapOp p =
  forall type0 $ \tFrom ->
    forall type0 $ \tTo ->
      (tFrom ~> tTo) ~> tList tFrom ~> tList tTo

typeOfFoldOp :: Provenance -> DSLExpr
typeOfFoldOp p =
  forall type0 $ \tFrom ->
    forall type0 $ \tTo ->
      (tFrom ~> tTo ~> tTo) ~> tTo ~> tList tFrom ~> tTo