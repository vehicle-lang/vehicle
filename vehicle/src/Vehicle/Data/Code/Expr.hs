{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Code.Expr
  ( Expr (Universe, Pi, Builtin, BoundVar, FreeVar, Hole, Meta, Let, Lam, Record, RecordAcc, App),
    Type,
    Binder,
    Arg,
    Telescope,
    Decl,
    Prog,
    normAppList,
    normApp,
    isTypeSynonym,
    mkHole,
    pattern TypeUniverse,
    pattern BuiltinExpr,
    BuiltinUpdate,
    traverseBuiltinsM,
    mapBuiltins,
    FreeVarUpdate,
    traverseFreeVarsM,
    freeVarsIn,
    substDBAll,
    substDBInto,
    substArgs,
    liftDBIndices,
    Substitution,
    substituteDB,
  )
where

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
import Vehicle.Data.Code.Interface (HasBuiltinConstructor (..))
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Data.Variable.Bound.Index (Ix (..))
import Vehicle.Data.Variable.Bound.Level (Lv, unLv)
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
  | -- | Records
    Record
      Provenance
      Identifier
      (RecordFields (Expr builtin))
  | -- | Records accessors
    RecordAcc
      Provenance
      (Expr builtin)
      (Identifier, FieldName)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

--------------------------------------------------------------------------------
-- Safe applications

-- | Smart constructor for applications.
normApp :: Expr builtin -> NonEmpty (Arg builtin) -> Expr builtin
normApp (UnsafeApp f xs) ys = UnsafeApp f (xs <> ys)
normApp f xs = UnsafeApp f xs

-- | Safe pattern synonym for applications.
pattern App :: Expr builtin -> NonEmpty (Arg builtin) -> Expr builtin
pattern App f xs <- UnsafeApp f xs
  where
    App f xs = normApp f xs

{-# COMPLETE Universe, App, Pi, Builtin, BoundVar, FreeVar, Hole, Meta, Let, Lam, Record, RecordAcc #-}

-- | Smart constructor for applications with possibly no arguments.
normAppList :: Expr builtin -> [Arg builtin] -> Expr builtin
normAppList f [] = f
normAppList f (x : xs) = App f (x :| xs)

--------------------------------------------------------------------------------
-- The AST datatypes specialised to the Expr type

type Type builtin = Expr builtin

type Binder builtin = GenericBinder (Expr builtin)

type Arg builtin = GenericArg (Expr builtin)

type Telescope builtin = [Binder builtin]

type Decl builtin = GenericDecl (Expr builtin)

type Prog builtin = GenericProg (Expr builtin)

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
    Record p _ _ -> p
    RecordAcc p _ _ -> p

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

getBuiltinApp :: Expr builtin -> Maybe (builtin, [Arg builtin])
getBuiltinApp = \case
  Builtin _ b -> Just (b, [])
  App (Builtin _ b) args -> Just (b, NonEmpty.toList args)
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
  Record p i fs -> Record p i <$> traverseRecordFields (traverseBuiltinsM f) fs
  RecordAcc p r field -> RecordAcc p <$> traverseBuiltinsM f r <*> pure field
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
      Record p i fs -> Record p i <$> traverseRecordFields go fs
      RecordAcc p r field -> RecordAcc p <$> go r <*> pure field

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

instance (BuiltinHasBinders builtin) => HasBinders (Expr builtin) where
  getBinder = \case
    Pi _ binder body -> Just (PiBinder, binder, body)
    Lam _ binder body -> Just (LamBinder, binder, body)
    BuiltinExpr _ (getBuiltinBinder -> Just b) (NonEmpty.last -> (argExpr -> Lam _ binder body)) -> Just (b, binder, body)
    _ -> Nothing

  getLetBinder = \case
    Let _ value binder body -> Just (value, binder, body)
    _ -> Nothing

instance HasBuiltinConstructor Expr where
  accessBuiltinC =
    Access
      { getExpr = getBuiltinApp,
        mkExpr = \(b, args) -> normAppList (Builtin mempty b) args
      }

--------------------------------------------------------------------------------
-- DeBruijin substitution

type Substitution value = Ix -> Either Ix value

class Substitutable value target | target -> value where
  subst :: (MonadReader (Lv, Substitution value) m) => target -> m target

instance (Substitutable expr expr) => Substitutable expr (GenericArg expr) where
  subst = traverse subst

instance (Substitutable expr expr) => Substitutable expr (GenericBinder expr) where
  subst = traverse subst

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
    Record p i fs -> Record p i <$> traverseRecordFields subst fs
    RecordAcc p r field -> RecordAcc p <$> subst r <*> pure field

shiftDBIndex :: Ix -> Lv -> Ix
shiftDBIndex i l = Ix (unIx i + unLv l)

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
