{-# OPTIONS_GHC -Wno-orphans #-}
module Vehicle.Core.Compile.Type where

import Control.Monad (when, unless, zipWithM_)
import Control.Monad.Except (MonadError(..), Except)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (MonadState(..), StateT(..), modify)
import Data.Text (Text)
import Data.Foldable (toList, foldrM)
import Text.Printf (printf)
import Data.List (foldl')
import Data.List.NonEmpty qualified as NonEmpty (scanl, head)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Coerce (coerce)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.List.NonEmpty (NonEmpty(..))
import Prettyprinter ( (<+>), Pretty(pretty), encloseSep, lbracket, rbracket, comma )

import Vehicle.Core.AST hiding (lift)
import Vehicle.Core.Compile.DSL
-- import Vehicle.Core.Print ( prettyInfo )
import Vehicle.Prelude

-- TODO:
--
--  - Extend contexts with information for meta-variables.
--  - Add support for unification with meta-variables.
--

-- * Errors thrown during type checking

data TypingError
  = IndexOutOfBounds
    Provenance    -- ^ The location of the deBruijn index.
    Index         -- ^ The deBruijn index.
  | UnresolvedHole
    Provenance    -- ^ The location of the hole
    Symbol        -- ^ The name of the hole
  | Mismatch
    Provenance    -- ^ The location of the mismatch.
    CheckedExpr   -- ^ The possible inferred types.
    CheckedExpr   -- ^ The expected type.
  | MissingAnnotation
    Provenance    -- ^ The location of the missing annotation.
  | UnsupportedOperation
    Provenance    -- ^ The location of the unsupported operation.
    Text          -- ^ A description of the unsupported operation.
{-
instance MeaningfulError TypeError where
  details (IndexOutOfBounds p index) = DError $ DeveloperError
    { provenance = p
    , problem    = "DeBruijn index" <+> pretty index <+> "out of bounds"
    }

  details (Mismatch p candidates expected) = UError $ UserError
    { provenance = p
    , problem    = "expected something of type" <+> prettyInfo expected <+>
                   "but inferred types" <+> encloseSep lbracket rbracket comma (map prettyInfo candidates)
    , fix        = "unknown"
    }

  details (MissingAnnotation p) = DError $ DeveloperError
    { provenance = p
    , problem    = "missing annotation"
    }

  details (UnsupportedOperation p t) = UError $ UserError
    { provenance = p
    , problem    = "type-checking of" <+> squotes t <+> "not currently supported"
    , fix        = "unknown"
    }
-}
-- * Type and kind contexts

data UnificationConstraint = Unify Provenance CheckedExpr CheckedExpr
data TypeClassConstraint   = Meta `Has` CheckedExpr

-- | The declarations that are currently in scope, indexed into via their names.
type DeclCtx  = Map Identifier CheckedExpr

instance Pretty DeclCtx where
  pretty = pretty . show

-- | The expression variables that are in currently in scope, indexed into via De Bruijn expressions.
type BoundCtx = Seq CheckedExpr

instance Pretty BoundCtx where
  pretty = pretty . show

data MetaCtx  = MetaCtx
  { nextMeta               :: Meta
  , metaVariableTypes      :: IntMap CheckedExpr
  , unificationConstraints :: [UnificationConstraint]
  , typeClassConstraints   :: [TypeClassConstraint]
  }

type TCM a =
  StateT MetaCtx
    (ReaderT BoundCtx
      (ReaderT DeclCtx
        (Except TypingError)))
          a

getMetaCtx :: TCM MetaCtx
getMetaCtx = get

modifyMetaCtx :: (MetaCtx -> MetaCtx) -> TCM ()
modifyMetaCtx = modify

addUnificationConstraints :: [UnificationConstraint] -> TCM ()
addUnificationConstraints cs = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { unificationConstraints = cs <> unificationConstraints, ..}

unify :: Provenance -> CheckedExpr -> CheckedExpr -> TCM CheckedExpr
unify p e1 e2 = do
  addUnificationConstraints [Unify p e1 e2]
  -- TODO calculate the most general unifier
  return e1

addTypeClassConstraints :: [TypeClassConstraint] -> TCM ()
addTypeClassConstraints ts = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { typeClassConstraints = ts <> typeClassConstraints, ..}

getMetaType :: Meta -> TCM CheckedExpr
getMetaType i = do
  ctx <- getMetaCtx;
  case IntMap.lookup i (metaVariableTypes ctx) of
    Just typ -> return typ
    Nothing  -> developerError ("Meta variable ?" <> pretty i <+> "not found in context")

getBoundCtx :: TCM BoundCtx
getBoundCtx = ask

modifyBoundCtx :: (BoundCtx -> BoundCtx) -> TCM a -> TCM a
modifyBoundCtx = local

getDeclCtx :: TCM DeclCtx
getDeclCtx = lift (lift ask)

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
freshMeta :: CheckedExpr -> TCM (Meta, CheckedExpr)
freshMeta resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx  <- getBoundCtx
  let metaType = foldl' (tPiInternal Explicit) resultType boundCtx

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv =
        [ Var (makeTypeAnn varType) (Bound varIndex)
        | (varIndex , varType) <- zip [0..] (toList boundCtx) ]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' app (Meta _ metaName) boundEnv
  return (metaName, meta)

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshUncheckedMeta :: CheckedExpr -> TCM (Meta, UncheckedExpr)
freshUncheckedMeta resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx  <- getBoundCtx
  let metaType = foldl' (tPiInternal Explicit) resultType boundCtx

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv = [ Var mempty (Bound varIndex) | varIndex <- [0..length boundCtx - 1]]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' (\f x -> App mempty f (Arg mempty Explicit x)) (Meta _ metaName) boundEnv
  return (metaName, meta)

viaInfer :: Provenance -> CheckedExpr -> UncheckedExpr -> TCM CheckedExpr
viaInfer p expectedType e = do
  -- TODO may need to change the term when unifying to insert a type application.
  (e', actualType) <- infer e
  _t' <- unify p expectedType actualType
  return e'

check :: CheckedExpr     -- Type we're checking against
      -> UncheckedExpr   -- Expression being type-checked
      -> TCM CheckedExpr -- Updated expression
check expectedType = \case
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

  Hole _p _name -> do
    -- Replace the hole with meta-variable of the expected type.
    -- NOTE, different uses of the same hole name will be interpreted as different meta-variables.
    (_, meta) <- freshMeta expectedType
    return meta

  e@(Lam p (Binder pBound vBound nBound tBound1) body) ->
    case expectedType of
      -- Check if it's a Pi type which is expecting the right visibility
      Pi _ (Binder _ vFun _ tBound2) tRes
        | vBound == vFun -> do
          modifyBoundCtx (tBound2 Seq.<|) $ do
            body' <- check tRes body
            (tBound1' , _) <- infer tBound1
            tBound' <- unify p tBound1' tBound2
            return $ Lam (RecAnn expectedType p) (Binder pBound vBound nBound tBound') body'

      -- Check if it's a Pi type which is expecting an implicit argument
      -- but is being applied to an explicit argument
      Pi _ (Binder _ Implicit name tBound2) tRes -> do
        -- Add the implict argument to the context
        modifyBoundCtx (tBound2 Seq.<|) $ do
          -- Check if the type matches the expected result type.
          e' <- check tRes e

          -- Create a new binder mirroring the implicit Pi binder expected
          let newBinder = Binder mempty Implicit name tBound2

          -- Prepend a new lambda to the expression with the implicit binder
          return $ Lam (RecAnn expectedType mempty) newBinder e'

      _ -> do
        let expected = tPiInternal vBound (hole "a") (hole "b")
        throwError $ Mismatch p expectedType expected

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
infer :: UncheckedExpr
      -> TCM (CheckedExpr, CheckedExpr)
infer = \case
  Type l ->
    return (Type l , Type (l + 1))

  Constraint ->
    return (Constraint , Type 1)

  Meta p i -> do
    metaType <- getMetaType i
    return (Meta p i , metaType)

  Hole ann s ->
    throwError $ UnresolvedHole (prov ann) s

  Ann ann e t   -> do
    (t', _) <- infer t
    e' <- check t' e
    let ann' = RecAnn t' ann
    return (Ann ann' e' t' , t')

  App p fun arg@(Arg pArg vArg eArg) -> do
    -- Infer the type of the function.
    (fun' , tFun') <- infer fun

    case tFun' of
      -- Check if it's a Pi type which is expecting the right visibility
      Pi _ (Binder _ vFun _ tArg) tRes
        | vArg == vFun -> do
          -- Check the type of the argument.
          eArg' <- check tArg eArg

          -- Return the appropriately annotated type with its inferred kind.
          return (App (RecAnn tRes p) fun' (Arg pArg vArg eArg') , tRes)

      -- Check if it's a Pi type which is expecting an implicit argument
      -- but is being applied to an explicit argument
      Pi _ (Binder _ Implicit _ tArg) _tRes -> do
        -- Generate a fresh meta variable
        (meta, metaArg) <- freshUncheckedMeta tArg

        -- TODO Wen is worried about interactions between the Pi abstractions over
        -- the context and the type-class search later on.

        -- Check if the implicit argument is a type-class
        when (isConstraint tArg) $
          addTypeClassConstraints [meta `Has` tArg]

        -- Apply the function to the new meta.
        let funWithImplicit = App mempty fun (Arg mempty Implicit metaArg)

        -- Try again to infer the type of the application.
        infer (App p funWithImplicit arg)

      _ -> do
        let expected = tPiInternal vArg (hole "a") (hole "b")
        throwError $ Mismatch p tFun' expected


  Pi p (Binder pBound vis name arg) res -> do
    (arg', tArg') <- infer arg

    modifyBoundCtx (arg' Seq.<|) $ do
      (res', tRes') <- infer res
      let t' = tArg' `tMax` tRes'
      return (Pi (RecAnn t' p) (Binder pBound vis name arg') res' , t')

  Builtin p op -> do
    let t' = typeOfBuiltin p op
    return (Builtin (RecAnn t' p) op, t')

  Var p (Bound i) -> do
    -- Lookup the type of the variable in the context.
    ctx <- getBoundCtx
    case ctx Seq.!? i of
      Just t' -> return (Var (RecAnn t' p) (Bound i), t')
      Nothing -> developerError $
        "Index" <+> pretty i <+> "out of bounds when looking up variable in context" <+> pretty ctx <+> "at" <+> pretty p

  Var p (Free ident) -> do
    -- Lookup the type of the declaration variable in the context.
    ctx <- getDeclCtx
    case Map.lookup ident ctx of
      Just t' -> return (Var (RecAnn t' p) (Free ident), t')
      -- This should have been caught during scope checking
      Nothing -> developerError $
        "Declaration'" <+> pretty ident <+> "'not found when looking up variable in context" <+> pretty ctx <+> "at" <+> pretty p

  Let p (Binder pBound vis name tBound) arg body -> do
    -- Infer the type of the let arg from the annotation on the binder
    (tBound', _) <- infer tBound

    -- Check the arg actually has that type
    arg' <- check tBound' arg

    -- Update the context with the bound variable
    modifyBoundCtx (tBound' Seq.<|) $ do
      -- Infer the type of the body
      (body' , tBody') <- infer body
      return (Let (RecAnn tBody' p) (Binder pBound vis name tBound') arg' body' , tBody')

  Lam p (Binder pBound vis name tBound) body -> do
    -- Infer the type of the bound variable from the binder
    (tBound', _) <- infer tBound

    -- Update the context with the bound variable
    modifyBoundCtx (tBound' Seq.<|) $ do
      (body' , tBody') <- infer body
      let t' = tPi pBound vis name tBound' tBody'
      return (Lam (RecAnn t' p) (Binder pBound vis name t') body' , t')

  Literal p l -> do
    let t' = typeOfLiteral p l
    return (Literal (RecAnn t' p) l, t')

  Seq p es -> do
    (es', ts') <- unzip <$> traverse infer es

    -- Generate a fresh meta variable for the case where the list is empty
    (_, tElem) <- freshMeta Type0

    -- Unify the types of all the elements in the list
    tElem' <- foldrM (unify p) tElem ts'

    -- Generate a meta-variable for the type of the container
    (meta, tCont') <- freshMeta Type0
    addTypeClassConstraints [meta `Has` isContainer p tCont' tElem']

    return (Seq (RecAnn tCont' p) es' , tCont')

inferDecl :: UncheckedDecl -> TCM CheckedDecl
inferDecl = \case
  DeclNetw p ident t      -> DeclNetw p ident . fst <$> infer t
  DeclData p ident t      -> DeclData p ident . fst <$> infer t
  DefFun   p ident t body -> do
      (t' , _) <- infer t
      body' <- check t' body
      return $ DefFun p ident t' body'

inferProg :: UncheckedProg -> TCM CheckedProg
inferProg (Main ds) = Main <$> traverse inferDecl ds


typeOfLiteral :: Provenance -> Literal -> CheckedExpr
typeOfLiteral p (LNat  _) = tForall Type0 $ \t -> isNatural p t ~~> t
typeOfLiteral p (LInt  _) = tForall Type0 $ \t -> isIntegral p t ~~> t
typeOfLiteral p (LReal _) = tForall Type0 $ \t -> isReal p t ~~> t
typeOfLiteral p (LBool _) = tForall Type0 $ \t -> isTruth p t ~~> t

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