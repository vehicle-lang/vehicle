{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Core.Compile.Type
  ( TypingError(..)
  , runTypeChecking
  ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError(..), Except, withExcept)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadTrans(..))
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
  let prog2 = runStateT prog1 emptyMetaCtx
  let prog3 = runReaderT prog2 mempty
  let prog4 = runReaderT prog3 mempty
  (checkedProg, metaCtx) <- prog4
  trace (layoutAsString $ pretty metaCtx) (return ())
  let constraints = unificationConstraints metaCtx
  (unsolvedConstraints, metaSubst) <- withExcept UnificationError (solve (constraints, mempty))
  trace (show metaSubst) (return ())
  case unsolvedConstraints of
    []     -> return $ substMetas metaSubst checkedProg
    c : cs -> throwError $ UnsolvedConstraints (c :| cs)


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
    CheckedExpr             -- The expected type.
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
    , problem    = "expected something of type" <+> pretty expected <+>
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
    , problem    = ""
    , fix        = ""
    }

  details (UnificationError e) = details e

--------------------------------------------------------------------------------
-- Contexts

-- | The type-checking monad
type TCM a =
  StateT MetaCtx
    (ReaderT BoundCtx
      (ReaderT DeclCtx
        (Except TypingError)))
          a

-- | The declarations that are currently in scope, indexed into via their names.
type DeclCtx = Map Identifier CheckedExpr

instance Pretty DeclCtx where
  pretty = pretty . show

getDeclCtx :: TCM DeclCtx
getDeclCtx = lift (lift ask)


-- | The expression variables that are in currently in scope,
-- indexed into via De Bruijn expressions.
type BoundCtx = [(Name, CheckedExpr)]

getBoundCtx :: TCM BoundCtx
getBoundCtx = ask

addToBoundCtx :: Name -> CheckedExpr -> TCM a -> TCM a
addToBoundCtx n e = local ((n, e) :)

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

getMetaCtx :: TCM MetaCtx
getMetaCtx = get

modifyMetaCtx :: (MetaCtx -> MetaCtx) -> TCM ()
modifyMetaCtx = modify

addUnificationConstraints :: [UnificationConstraint] -> TCM ()
addUnificationConstraints cs = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { unificationConstraints = cs <> unificationConstraints, ..}

unify :: Provenance -> CheckedExpr -> CheckedExpr -> TCM CheckedExpr
unify p e1 e2 = do
  ctx <- getBoundCtx
  -- TODO calculate the context (currently breaks the printing of unification constraints)
  addUnificationConstraints [makeConstraint p ctx e1 e2]
  -- TODO calculate the most general unifier
  return e1

addTypeClassConstraints :: [TypeClassConstraint] -> TCM ()
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
freshMetaName :: TCM Meta
freshMetaName = do
  MetaCtx {..} <- getMetaCtx;
  put $ MetaCtx { nextMeta = succ nextMeta , ..}
  return nextMeta

-- TODO unify these functions in a pleasing way

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMeta :: Provenance -> CheckedExpr -> TCM (Meta, CheckedExpr)
freshMeta p resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx  <- getBoundCtx
  let metaType = foldl' (tPiInternal Explicit) resultType (fmap snd boundCtx)

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv =
        [ Var (makeTypeAnn varType) (Bound varIndex)
        | (varIndex , (_ , varType)) <- zip [0..] (toList boundCtx) ]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' app (Meta (RecAnn metaType p) metaName) boundEnv
  return (metaName, meta)

--------------------------------------------------------------------------------
-- Type-checking of expressions

showCheckEntry :: CheckedExpr -> UncheckedExpr -> UncheckedExpr
showCheckEntry t e = trace ("check-entry " <> showCore e <> " <= " <> showCore t) e

showCheckExit :: TCM CheckedExpr -> TCM CheckedExpr
showCheckExit m = do
  e <- m
  trace ("check-exit  " <> showCore e) (return e)

check :: CheckedExpr     -- Type we're checking against
      -> UncheckedExpr   -- Expression being type-checked
      -> TCM CheckedExpr -- Updated expression
check expectedType expr = showCheckExit $ case showCheckEntry expectedType expr of
  e@(Type _)          -> viaInfer mempty expectedType e
  e@Constraint        -> viaInfer mempty expectedType e
  e@(Meta _ _)        -> viaInfer mempty expectedType e
  e@(App     p _ _)   -> viaInfer p      expectedType e
  e@(Pi      p _ _)   -> viaInfer p      expectedType e
  e@(Builtin p _)     -> viaInfer p      expectedType e
  e@(Var     p _)     -> viaInfer p      expectedType e
  e@(Let     p _ _ _) -> viaInfer p      expectedType e
  e@(Literal p _)     -> viaInfer p      expectedType e
  e@(Seq     p _)     -> viaInfer p      expectedType e
  e@(Ann     p _ _)   -> viaInfer p      expectedType e

  Hole p _name -> do
    -- Replace the hole with meta-variable of the expected type.
    -- NOTE, different uses of the same hole name will be interpreted as different meta-variables.
    (_, meta) <- freshMeta p expectedType
    return meta

  e@(Lam p (Binder pBound vBound nBound tBound1) body) ->
    case expectedType of
      -- Check if it's a Pi type which is expecting the right visibility
      Pi _ (Binder _ vFun _ tBound2) tRes
        | vBound == vFun -> do
          tBound1' <- check (getType tBound2) tBound1
          tBound' <- unify p tBound2 tBound1'

          addToBoundCtx nBound tBound' $ do
            body' <- check tRes body
            return $ Lam (RecAnn expectedType p) (Binder pBound vBound nBound tBound') body'

      -- Check if it's a Pi type which is expecting an implicit argument
      -- but is being applied to an explicit argument
      Pi _ (Binder _ Implicit name tBound2) tRes -> do
        -- Add the implict argument to the context
        addToBoundCtx nBound tBound2 $ do
          -- Check if the type matches the expected result type.
          e' <- check tRes e

          -- Create a new binder mirroring the implicit Pi binder expected
          let newBinder = Binder mempty Implicit name tBound2

          -- Prepend a new lambda to the expression with the implicit binder
          return $ Lam (RecAnn expectedType mempty) newBinder e'

      _ -> do
        let expected = tPiInternal vBound (hole "a") (hole "b")
        throwError $ Mismatch p expectedType expected


inferApp :: Provenance -> UncheckedArg -> CheckedExpr -> CheckedExpr -> TCM (CheckedExpr, CheckedExpr)
inferApp p arg@(Arg pArg vArg eArg) fun' = \case
  -- Check if it's a Pi type which is expecting the right visibility
  Pi _ (Binder _ vFun _ tArg') tRes'
    | vArg == vFun -> do
      -- Check the type of the argument.
      eArg' <- check tArg' eArg

      -- Substitute argument in tRes
      let tResSubst' = eArg' `substInto` tRes'

      -- Return the appropriately annotated type with its inferred kind.
      return (App (RecAnn tResSubst' p) fun' (Arg pArg vArg eArg') , tResSubst')

  -- Check if it's a Pi type which is expecting an implicit argument
  -- but is being applied to an explicit argument
  Pi _ (Binder _ Implicit _ tArg') tRes' -> do
    -- Generate a fresh meta variable
    (meta, metaArg) <- freshMeta p tArg'

    -- Substitute meta-variable in tRes
    let tResSubst' = metaArg `substInto` tRes'

    -- TODO Wen is worried about interactions between the Pi abstractions over
    -- the context and the type-class search later on.

    -- Check if the implicit argument is a type-class
    when (isConstraint tArg') $
      addTypeClassConstraints [meta `Has` tArg']

    -- Apply the function to the new meta and try again to infer the type of the application.
    inferApp p arg (App (RecAnn tResSubst' mempty) fun' (Arg mempty Implicit metaArg)) tResSubst'

  tFun' -> do
    let expected = tPiInternal vArg (hole "a") (hole "b")
    throwError $ Mismatch p tFun' expected


showInferEntry :: UncheckedExpr -> UncheckedExpr
showInferEntry e = trace ("infer-entry " <> showCore e) e

showInferExit :: TCM (CheckedExpr, CheckedExpr) -> TCM (CheckedExpr, CheckedExpr)
showInferExit m = do
  (e, t) <- m
  trace ("infer-exit  " <> showCore e <> " => " <> showCore t) (return (e,t))

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
infer :: UncheckedExpr
      -> TCM (CheckedExpr, CheckedExpr)
infer e = showInferExit $ case showInferEntry e of
  Type l ->
    return (Type l , Type (l + 1))

  Constraint ->
    return (Constraint , Type 1)

  Meta _ m -> developerError $ "Trying to infer the type of a meta-variable" <+> pretty m

  Hole ann s ->
    throwError $ UnresolvedHole (prov ann) s

  Ann ann e t   -> do
    (t', _) <- infer t
    e' <- check t' e
    let ann' = RecAnn t' ann
    return (Ann ann' e' t' , t')

  App p fun arg -> do
    -- Infer the type of the function.
    (fun' , tFun') <- infer fun
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
        let t'' = repeatN liftDBIndices (i+1) t'
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
      let t' = tPi pBound vis name tBound' tBody'
      return (Lam (RecAnn t' p) (Binder pBound vis name t') body' , t')

  Literal p l -> do
    let t' = typeOfLiteral p l
    return (Literal (RecAnn t' p) l, t')

  Seq p es -> do
    (es', ts') <- unzip <$> traverse infer es

    -- Generate a fresh meta variable for the case where the list is empty
    (_, tElem) <- freshMeta p Type0

    -- Unify the types of all the elements in the list
    tElem' <- foldrM (unify p) tElem ts'

    -- Generate a meta-variable for the type of the container
    (meta, tCont') <- freshMeta p Type0
    addTypeClassConstraints [meta `Has` isContainer p tCont' tElem']

    return (Seq (RecAnn tCont' p) es' , tCont')

inferDecl :: UncheckedDecl -> TCM CheckedDecl
inferDecl = \case
  DeclNetw p ident t      -> DeclNetw p ident . fst <$> infer t
  DeclData p ident t      -> DeclData p ident . fst <$> infer t
  DefFun   p ident t body -> do
      -- TODO: check that 't' is actually a type
      (t' , _) <- infer t
      body' <- check t' body
      return $ DefFun p ident t' body'

inferProg :: UncheckedProg -> TCM CheckedProg
inferProg (Main ds) = Main <$> traverse inferDecl ds

viaInfer :: Provenance -> CheckedExpr -> UncheckedExpr -> TCM CheckedExpr
viaInfer p expectedType e = do
  -- TODO may need to change the term when unifying to insert a type application.
  (e', actualType) <- infer e
  _t' <- unify p expectedType actualType
  return e'

--------------------------------------------------------------------------------
-- Typing of literals

typeOfLiteral :: Provenance -> Literal -> CheckedExpr
typeOfLiteral p (LNat  _) = tForall Type0 $ \t -> isNatural p t ~~> t
typeOfLiteral p (LInt  _) = tForall Type0 $ \t -> isIntegral p t ~~> t
typeOfLiteral p (LReal _) = tForall Type0 $ \t -> isReal p t ~~> t
typeOfLiteral p (LBool _) = tForall Type0 $ \t -> isTruth p t ~~> t

--------------------------------------------------------------------------------
-- Typing of builtins

-- |Return the kind for builtin exprs.
typeOfBuiltin :: Provenance -> Builtin -> CheckedExpr
typeOfBuiltin p = \case
  Bool            -> Type0
  Prop            -> Type0
  Nat             -> Type0
  Int             -> Type0
  Real            -> Type0
  List            -> Type0 ~> Type0
  Tensor          -> Type0 ~> tList tNat ~> Type0

  HasEq           -> Type0 ~> Type0 ~> Constraint
  HasOrd          -> Type0 ~> Type0 ~> Constraint
  IsTruth         -> Type0 ~> Constraint
  IsNatural       -> Type0 ~> Constraint
  IsIntegral      -> Type0 ~> Constraint
  IsRational      -> Type0 ~> Constraint
  IsReal          -> Type0 ~> Constraint
  IsContainer     -> Type0 ~> Constraint
  IsQuantifiable  -> Type0 ~> Type0 ~> Constraint

  If   -> tForall Type0 $ \t -> tProp ~> t ~> t
  Cons -> tForall Type0 $ \t -> t ~> tList t ~> tList t

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

  All  -> typeOfQuantifierOp p
  Any  -> typeOfQuantifierOp p

typeOfEqualityOp :: Provenance -> CheckedExpr
typeOfEqualityOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      hasEq p t r ~~> t ~> t ~> r

typeOfComparisonOp :: Provenance -> CheckedExpr
typeOfComparisonOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      hasOrd p t r ~~> t ~> t ~> r

typeOfBoolOp2 :: Provenance -> CheckedExpr
typeOfBoolOp2 p =
  tForall Type0 $ \t ->
    isTruth p t ~~> t ~> t ~> t

typeOfBoolOp1 :: Provenance -> CheckedExpr
typeOfBoolOp1 p =
  tForall Type0 $ \t ->
    isTruth p t ~~> t ~> t

typeOfNumOp2 :: (CheckedExpr -> CheckedExpr) -> CheckedExpr
typeOfNumOp2 numConstraint =
  tForall Type0 $ \t ->
    numConstraint t ~~> t ~> t ~> t

typeOfNumOp1 :: (CheckedExpr -> CheckedExpr) -> CheckedExpr
typeOfNumOp1 numConstraint =
  tForall Type0 $ \t ->
    numConstraint t ~~> t ~> t

typeOfQuantifierOp :: Provenance -> CheckedExpr
typeOfQuantifierOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      isQuantifiable p t r ~~> t ~> (t ~> r) ~> r

typeOfAtOp :: Provenance -> CheckedExpr
typeOfAtOp p =
  tForall Type0 $ \tCont ->
    tForall Type0 $ \tElem ->
      isContainer p tCont tElem ~~> tCont ~> tNat ~> tElem
