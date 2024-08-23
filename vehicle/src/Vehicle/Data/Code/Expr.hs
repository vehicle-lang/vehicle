{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Code.Expr where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), runReader)
import Control.Monad.Writer (MonadWriter (..), execWriter)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize (Serialize)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface
import Vehicle.Data.DeBruijn (Ix (..), Lv, Substitutable (..), Substitution, shiftDBIndex, unLv)
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Prelude
import Vehicle.Syntax.Sugar (BinderType (..), HasBinders (..))

--------------------------------------------------------------------------------
-- Expressions

-- | Type of Vehicle internal expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr builtin
  = -- | A universe, used to type types.
    Universe
      Provenance
      UniverseLevel
  | -- | Application of one term to another. Doesn't have provenance as it has no syntax in the grammar.
    UnsafeApp
      (Expr builtin) -- Function.
      (NonEmpty (Arg builtin)) -- Arguments.
  | -- | Dependent product (subsumes both functions and universal quantification).
    Pi
      Provenance
      (Binder builtin) -- The bound name
      (Expr builtin) -- (Dependent) result type.
  | -- | Terms consisting of constants that are built into the language.
    Builtin
      Provenance
      builtin -- Builtin name.
  | -- | Variables that are bound locally by other expressions
    BoundVar
      Provenance
      Ix -- Variable name.
  | -- | Variables that refer to other declarations
    FreeVar
      Provenance
      Identifier -- Declaration name
  | -- | A hole in the program.
    Hole
      Provenance
      Name -- Hole name.
  | -- | Unsolved meta variables.
    Meta
      Provenance
      MetaID -- Meta variable number.
  | -- | Let expressions. We have these in the core syntax because we want to
    -- cross compile them to various backends.
    --
    -- NOTE: that the order of the bound expression and the binder is reversed
    -- to better mimic the flow of the context, which makes writing monadic
    -- operations concisely much easier.
    Let
      Provenance
      (Expr builtin) -- Bound expression body.
      (Binder builtin) -- Bound expression name.
      (Expr builtin) -- Expression body.
  | -- | Lambda expressions (i.e. anonymous functions).
    Lam
      Provenance
      (Binder builtin) -- Bound expression name.
      (Expr builtin) -- Expression body.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

--------------------------------------------------------------------------------
-- The AST datatypes specialised to the Expr type

type Type builtin = Expr builtin

type Binder builtin = GenericBinder (Expr builtin)

type Arg builtin = GenericArg (Expr builtin)

type Telescope builtin = [Binder builtin]

type Decl builtin = GenericDecl (Expr builtin)

type Prog builtin = GenericProg (Expr builtin)

--------------------------------------------------------------------------------
-- Safe applications

-- | Smart constructor for applications with possibly no arguments.
normAppList :: Expr builtin -> [Arg builtin] -> Expr builtin
normAppList f [] = f
normAppList f (x : xs) = App f (x :| xs)

-- | Smart constructor for applications.
normApp :: Expr builtin -> NonEmpty (Arg builtin) -> Expr builtin
normApp (UnsafeApp f xs) ys = UnsafeApp f (xs <> ys)
normApp f xs = UnsafeApp f xs

-- | Safe pattern synonym for applications.
pattern App :: Expr builtin -> NonEmpty (Arg builtin) -> Expr builtin
pattern App f xs <- UnsafeApp f xs
  where
    App f xs = normApp f xs

{-# COMPLETE Universe, App, Pi, Builtin, BoundVar, FreeVar, Hole, Meta, Let, Lam #-}

--------------------------------------------------------------------------------
-- Instances

instance (NFData builtin) => NFData (Expr builtin)

instance (Serialize builtin) => Serialize (Expr builtin)

instance HasProvenance (Expr builtin) where
  provenanceOf = \case
    Universe p _ -> p
    Hole p _ -> p
    Meta p _ -> p
    App e xs -> fillInProvenance [provenanceOf e, provenanceOf xs]
    Pi p _ _ -> p
    Builtin p _ -> p
    BoundVar p _ -> p
    FreeVar p _ -> p
    Let p _ _ _ -> p
    Lam p _ _ -> p

--------------------------------------------------------------------------------
-- Utilities

mkHole :: Provenance -> Name -> Expr builtin
mkHole p name = Hole p ("_" <> name)

-- | Tests if a definition's type indicates that the definition is a type
-- synonym.
isTypeSynonym :: Type builtin -> Bool
isTypeSynonym = \case
  Universe {} -> True
  Pi _ _ res -> isTypeSynonym res
  _ -> False

pattern TypeUniverse :: Provenance -> Int -> Expr builtin
pattern TypeUniverse p l = Universe p (UniverseLevel l)

pattern BuiltinExpr ::
  Provenance ->
  builtin ->
  NonEmpty (Arg builtin) ->
  Expr builtin
pattern BuiltinExpr p b args <- App (Builtin p b) args
  where
    BuiltinExpr p b args = App (Builtin p b) args

getBuiltinApp :: Expr builtin -> Maybe (Provenance, builtin, [Arg builtin])
getBuiltinApp = \case
  Builtin p b -> Just (p, b, [])
  App (Builtin p b) args -> Just (p, b, NonEmpty.toList args)
  _ -> Nothing

getFreeVarApp :: Expr builtin -> Maybe (Provenance, Identifier, [Arg builtin])
getFreeVarApp = \case
  FreeVar p b -> Just (p, b, [])
  App (FreeVar p b) args -> Just (p, b, NonEmpty.toList args)
  _ -> Nothing

-----------------------------------------------------------------------------
-- Traversing builtins

-- | Function for updating a builtin application
type BuiltinUpdate m builtin1 builtin2 =
  Provenance -> builtin1 -> [Arg builtin2] -> m (Expr builtin2)

-- | Traverses all the auxiliary type arguments in the provided element,
-- applying the provided update function when it finds them (or a space
-- where they should be).
traverseBuiltinsM ::
  (Monad m) =>
  BuiltinUpdate m builtin1 builtin2 ->
  Expr builtin1 ->
  m (Expr builtin2)
traverseBuiltinsM f expr = case expr of
  Builtin p b -> f p b []
  App (Builtin p b) args -> do
    args' <- traverse (traverseBuiltinsArg f) args
    f p b (NonEmpty.toList args')
  App fun args -> App <$> traverseBuiltinsM f fun <*> traverse (traverseBuiltinsArg f) args
  Pi p binder res -> Pi p <$> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f res
  Let p bound binder body -> Let p <$> traverseBuiltinsM f bound <*> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f body
  Lam p binder body -> Lam p <$> traverseBuiltinsBinder f binder <*> traverseBuiltinsM f body
  Universe p u -> return $ Universe p u
  FreeVar p v -> return $ FreeVar p v
  BoundVar p v -> return $ BoundVar p v
  Hole p n -> return $ Hole p n
  Meta p m -> return $ Meta p m

traverseBuiltinsArg :: (Monad m) => BuiltinUpdate m builtin1 builtin2 -> Arg builtin1 -> m (Arg builtin2)
traverseBuiltinsArg f = traverse (traverseBuiltinsM f)

traverseBuiltinsBinder :: (Monad m) => BuiltinUpdate m builtin1 builtin2 -> Binder builtin1 -> m (Binder builtin2)
traverseBuiltinsBinder f = traverse (traverseBuiltinsM f)

mapBuiltins ::
  (Provenance -> builtin1 -> [Arg builtin2] -> Expr builtin2) ->
  Expr builtin1 ->
  Expr builtin2
mapBuiltins f e = runIdentity (traverseBuiltinsM (\p b args -> return $ f p b args) e)

-----------------------------------------------------------------------------
-- Traversing free variables

-- | Function for updating a free variable application
type FreeVarUpdate m builtin =
  (Expr builtin -> m (Expr builtin)) ->
  Provenance ->
  Identifier ->
  [Arg builtin] ->
  m (Expr builtin)

-- | Traverses all the free variables in the provided element,
-- applying the provided update function when it finds them (or a space
-- where they should be).
traverseFreeVarsM ::
  forall m builtin.
  (Monad m) =>
  (Binder builtin -> m (Expr builtin) -> m (Expr builtin)) ->
  FreeVarUpdate m builtin ->
  Expr builtin ->
  m (Expr builtin)
traverseFreeVarsM underBinder processFreeVar = go
  where
    go :: Expr builtin -> m (Expr builtin)
    go expr = case expr of
      FreeVar p ident -> do
        processFreeVar go p ident mempty
      App (FreeVar p ident) args -> do
        processFreeVar go p ident (NonEmpty.toList args)
      App fun args -> do
        fun' <- go fun
        args' <- traverse (traverse go) args
        return $ App fun' args'
      BoundVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Pi p binder res -> do
        binder' <- traverse go binder
        res' <- underBinder binder' (go res)
        return $ Pi p binder' res'
      Lam p binder body -> do
        binder' <- traverse go binder
        body' <- underBinder binder' (go body)
        return $ Lam p binder' body'
      Let p bound binder body -> do
        bound' <- go bound
        binder' <- traverse go binder
        body' <- underBinder binder' (go body)
        return $ Let p bound' binder' body'

freeVarsIn :: Expr builtin -> Set Identifier
freeVarsIn =
  execWriter
    . traverseFreeVarsM
      (const id)
      ( \recGo p i args -> do
          args' <- traverse (traverse recGo) args
          tell $ Set.singleton i
          return $ normAppList (FreeVar p i) args'
      )

-----------------------------------------------------------------------------
-- Instances

instance (BuiltinHasStandardData builtin) => HasBinders (Expr builtin) where
  getBinder = \case
    Pi _ binder body -> Just (PiBinder, binder, body)
    Lam _ binder body -> Just (LamBinder, binder, body)
    IInfiniteQuantifier q _ (Lam _ binder body) -> Just (QuantifierBinder q, binder, body)
    _ -> Nothing

  getLetBinder = \case
    Let _ value binder body -> Just (value, binder, body)
    _ -> Nothing

instance (BuiltinHasStandardTypes builtin) => HasStandardTypes (Expr builtin) where
  mkType p b = normAppList (Builtin p (mkBuiltinType b))
  getType e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinType b of
      Just t -> Just (p, t, args)
      Nothing -> Nothing
    _ -> Nothing

instance (BuiltinHasStandardData builtin) => HasStandardData (Expr builtin) where
  mkFunction p b = normAppList (Builtin p (mkBuiltinFunction b))
  getFunction e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinFunction b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

  mkConstructor p b = normAppList (Builtin p (mkBuiltinConstructor b))
  getConstructor e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinConstructor b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

  mkFreeVar p ident = normAppList (FreeVar p ident)
  getFreeVar e = case getFreeVarApp e of
    Just (p, ident, args) -> Just (p, ident, args)
    _ -> Nothing

  getTypeClassOp e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinTypeClassOp b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

instance (BuiltinHasBoolLiterals builtin) => HasBoolLits (Expr builtin) where
  getBoolLit e = case e of
    Builtin _ (getBoolBuiltinLit -> Just b) -> Just (mempty, b)
    _ -> Nothing
  mkBoolLit p x = Builtin p (mkBoolBuiltinLit x)

instance (BuiltinHasIndexLiterals builtin) => HasIndexLits (Expr builtin) where
  getIndexLit e = case e of
    Builtin _ (getIndexBuiltinLit -> Just i) -> Just (mempty, i)
    _ -> Nothing
  mkIndexLit p i = Builtin p (mkIndexBuiltinLit i)

instance (BuiltinHasNatLiterals builtin) => HasNatLits (Expr builtin) where
  getNatLit e = case e of
    Builtin _ (getNatBuiltinLit -> Just n) -> Just (mempty, n)
    _ -> Nothing
  mkNatLit p n = Builtin p (mkNatBuiltinLit n)

instance (BuiltinHasRatLiterals builtin) => HasRatLits (Expr builtin) where
  getRatLit e = case e of
    Builtin _ (getRatBuiltinLit -> Just r) -> Just (mempty, r)
    _ -> Nothing
  mkRatLit p r = Builtin p (mkRatBuiltinLit r)

instance (BuiltinHasVecLiterals builtin) => HasStandardVecLits (Expr builtin) where
  getHomoVector = \case
    BuiltinExpr _ (getVecBuiltinLit -> Just {}) (t :| xs) -> Just (t, xs)
    _ -> Nothing
  mkHomoVector t xs = BuiltinExpr mempty (mkVecBuiltinLit (length xs)) (t :| xs)

instance (BuiltinHasListLiterals builtin) => HasStandardListLits (Expr builtin) where
  getNil = \case
    BuiltinExpr p (isBuiltinNil -> True) [t] -> Just (p, t)
    _ -> Nothing
  mkNil t = BuiltinExpr mempty mkBuiltinNil [t]

  getCons = \case
    BuiltinExpr p (isBuiltinCons -> True) [t, x, xs] -> Just (p, t, x, xs)
    _ -> Nothing
  mkCons t x xs = BuiltinExpr mempty mkBuiltinCons [t, x, xs]

instance (BuiltinHasRatType builtin) => HasRatType (Expr builtin) where
  getRatType e = case e of
    Builtin p (isRatBuiltinType -> True) -> Just p
    _ -> Nothing
  mkRatType p = Builtin p mkRatBuiltinType

instance (BuiltinHasVecType builtin) => HasVecType (Expr builtin) where
  getVectorType e = case e of
    BuiltinExpr p (isVecBuiltinType -> True) [t, n] -> Just (p, t, n)
    _ -> Nothing
  mkVectorType p t n = BuiltinExpr p mkVecBuiltinType [t, n]

instance (BuiltinHasRatTensor builtin) => HasRatTensors (Expr builtin) where
  getRatTensorOp e = case e of
    BuiltinExpr _ (getRatTensorBuiltin -> Just op) args -> Just (op, NonEmpty.toList args)
    _ -> Nothing
  mkRatTensorOp op = normAppList (Builtin mempty $ mkRatTensorBuiltin op)

instance (BuiltinHasBoolTensor builtin) => HasBoolTensors (Expr builtin) where
  getBoolTensorOp e = case e of
    BuiltinExpr _ (getBoolTensorBuiltin -> Just op) args -> Just (op, NonEmpty.toList args)
    _ -> Nothing
  mkBoolTensorOp op = normAppList (Builtin mempty $ mkBoolTensorBuiltin op)

instance (BuiltinHasDimensionTypes builtin) => HasDimensionTypes (Expr builtin) where
  getDimensionTypeOp e = case e of
    BuiltinExpr _ (getDimensionTypeBuiltin -> Just op) args -> Just (op, NonEmpty.toList args)
    _ -> Nothing
  mkDimensionTypeOp op = normAppList (Builtin mempty $ mkDimensionTypeBuiltin op)

instance (BuiltinHasDimensionData builtin) => HasDimensionData (Expr builtin) where
  getDimensionDataOp e = case e of
    BuiltinExpr _ (getDimensionDataBuiltin -> Just op) args -> Just (op, NonEmpty.toList args)
    _ -> Nothing
  mkDimensionDataOp op = normAppList (Builtin mempty $ mkDimensionDataBuiltin op)

--------------------------------------------------------------------------------
-- DeBruijin substitution

instance Substitutable (Expr builtin) (Expr builtin) where
  subst expr = case expr of
    BoundVar p i -> do
      (d, s) <- ask
      return $
        if unIx i < unLv d
          then BoundVar p i
          else case s (shiftDBIndex i (-d)) of
            Left i' -> BoundVar p (shiftDBIndex i' d)
            Right v -> if d > 0 then liftDBIndices d v else v
    Universe {} -> return expr
    Meta {} -> return expr
    Hole {} -> return expr
    Builtin {} -> return expr
    FreeVar {} -> return expr
    App fun args -> App <$> subst fun <*> traverse subst args
    Pi p binder res -> Pi p <$> traverse subst binder <*> underDBBinder (subst res)
    Let p e1 binder e2 -> Let p <$> subst e1 <*> traverse subst binder <*> underDBBinder (subst e2)
    Lam p binder e -> Lam p <$> traverse subst binder <*> underDBBinder (subst e)

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underDBBinder :: (MonadReader (Lv, c) m) => m a -> m a
underDBBinder = local (first (+ 1))

substituteDB :: Lv -> Substitution (Expr builtin) -> Expr builtin -> Expr builtin
substituteDB depth sub e = runReader (subst e) (depth, sub)

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftDBIndices ::
  -- | number of levels to lift by
  Lv ->
  -- | target term to lift
  Expr builtin ->
  -- | lifted term
  Expr builtin
liftDBIndices l = substituteDB 0 (\i -> Left (shiftDBIndex i l))

-- | De Bruijn aware substitution of one expression into another
substDBIntoAtLevel ::
  forall builtin.
  -- | The index of the variable of which to substitute
  Ix ->
  -- | expression to substitute
  Expr builtin ->
  -- | term to substitute into
  Expr builtin ->
  -- | the result of the substitution
  Expr builtin
substDBIntoAtLevel level value = substituteDB 0 substVar
  where
    substVar :: Ix -> Either Ix (Expr builtin)
    substVar v
      | v == level = Right value
      | v > level = Left (v - 1)
      | otherwise = Left v

-- | De Bruijn aware substitution of one expression into another
substDBInto ::
  -- | expression to substitute
  Expr builtin ->
  -- | term to substitute into
  Expr builtin ->
  -- | the result of the substitution
  Expr builtin
substDBInto = substDBIntoAtLevel 0

substDBAll ::
  Lv ->
  (Ix -> Maybe Ix) ->
  Expr builtin ->
  Expr builtin
substDBAll depth sub = substituteDB depth (\v -> maybe (Left v) Left (sub v))

substArgs :: Expr builtin -> [Arg builtin] -> Expr builtin
substArgs (Lam _ _ body) (arg : args) = do
  substArgs (argExpr arg `substDBInto` body) args
substArgs e args = normAppList e args
