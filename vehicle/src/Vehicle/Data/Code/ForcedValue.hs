module Vehicle.Data.Code.ForcedValue where

import Control.Monad (void, when)
import Control.Monad.Writer.Strict (MonadWriter (..), execWriter)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (traverse_)
import Data.Map.Ordered (OMap)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Internal.Check (HasCallStack)
import Data.Void (Void)
import GHC.Generics
import Vehicle.Data.AST.Expr.Scoped (Expr (..), traverseBoundVariables_)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Data.Variable.Bound.Context.Core
import Vehicle.Data.Variable.Bound.Context.Generic.Core
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Index (Ix)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

-----------------------------------------------------------------------------
-- Generic values
-----------------------------------------------------------------------------
-- Data types

-- | A normalised expression. This is made generic over the type of the meta
-- variables stored. This allows us to use a variant in type-checking with
-- meta-variables and a variant elsewhere without meta-variables.
data GenericForcedValue meta builtin
  = VUniverse
      !UniverseLevel
  | VMeta
      !meta
      !(GenericUnforcedSpine meta builtin)
  | VFreeVar
      !Identifier
      !(GenericUnforcedSpine meta builtin)
  | VBoundVar
      !Lv
      !(GenericUnforcedSpine meta builtin)
  | VRecordAcc
      !(GenericThunk meta builtin)
      !(GenericThunk meta builtin)
      !FieldName
      !(GenericUnforcedSpine meta builtin)
  | VBuiltin
      !builtin
      !(GenericUnforcedSpine meta builtin)
  | VLam
      !(GenericUnforcedBinder meta builtin)
      !(GenericClosure meta builtin)
  | VPi
      !(GenericUnforcedBinder meta builtin)
      !(GenericClosure meta builtin)
  | VRecord
      !(GenericThunk meta builtin)
      !(GenericUnforcedRecordFields meta builtin)
  deriving (Show, Generic, Eq, Ord)

-- | A thunk represents an expression that may not yet have been evaluated.
data GenericThunk meta builtin
  = Forced (GenericForcedValue meta builtin)
  | Unforced (GenericBoundEnv meta builtin) (Expr builtin)
  deriving (Show, Generic, Eq, Ord)

-- | The information stored for each variable in the environment. We choose
-- to store the binder as it's a convenient mechanism for passing through
-- name, relevance for pretty printing and debugging.
newtype GenericBoundEnv meta builtin = BoundEnv
  { unBoundEnv :: GenericBoundCtx (GenericBinder (), GenericThunk meta builtin)
  }
  deriving (Show, Eq, Ord)

-- | A closure represents the unevaluated body of a new binder
-- (e.g. lambda, pi, foreach, forall). A value for the bound variable
-- needs to be added to the contained environment in order to form an
-- evaluable `Thunk`.
data GenericClosure meta builtin = Closure (GenericBoundEnv meta builtin) (Expr builtin)
  deriving (Show, Generic, Eq, Ord)

type GenericUnforcedArg meta builtin = GenericArg (GenericThunk meta builtin)

-- | A list of arguments for an application that cannot be normalised.
type GenericUnforcedSpine meta builtin = [GenericUnforcedArg meta builtin]

type GenericUnforcedBinder meta builtin = GenericBinder (GenericThunk meta builtin)

type GenericUnforcedRecordFields meta builtin = OMap FieldName (GenericThunk meta builtin)

isVTypeUniverse :: GenericForcedValue meta builtin -> Bool
isVTypeUniverse = \case
  VUniverse {} -> True
  _ -> False

----------------------------------------------------------------------------
-- Operations over bound environments

emptyBoundEnv :: GenericBoundEnv meta builtin
emptyBoundEnv = BoundEnv mempty

unbound :: Lv -> GenericThunk meta builtin
unbound lv = Forced $ VBoundVar lv []

-- | Note that the `ctxSize` must come from the current context and not a
-- bound environment as the environment that the term was originally normalised
-- in may not be the same size as the current context.
extendEnvWithBound ::
  Lv ->
  GenericBinder expr ->
  GenericBoundEnv meta builtin ->
  GenericBoundEnv meta builtin
extendEnvWithBound ctxSize binder (BoundEnv env) =
  BoundEnv $ (void binder, unbound ctxSize) : env

extendEnvWithDefined ::
  GenericThunk meta builtin ->
  GenericBinder expr ->
  GenericBoundEnv meta builtin ->
  GenericBoundEnv meta builtin
extendEnvWithDefined value binder (BoundEnv env) =
  BoundEnv $ (void binder, value) : env

extendClosure :: GenericClosure meta builtin -> GenericUnforcedBinder meta builtin -> GenericThunk meta builtin -> GenericThunk meta builtin
extendClosure (Closure env expr) binder value = Unforced (extendEnvWithDefined value binder env) expr

extendClosureWithBound :: GenericClosure meta builtin -> GenericUnforcedBinder meta builtin -> Lv -> GenericThunk meta builtin
extendClosureWithBound (Closure env expr) binder lv = Unforced (extendEnvWithBound lv binder env) expr

lookupIxInEnv :: (HasCallStack) => GenericBoundEnv meta builtin -> Ix -> GenericThunk meta builtin
lookupIxInEnv (BoundEnv env) i = snd $ lookupIxInBoundCtx i env

boundContextToEnv :: BoundCtx expr -> GenericBoundEnv meta builtin
boundContextToEnv ctx = BoundEnv $ do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (bimap void unbound) numberedCtx

namedBoundContextToEnv :: NamedBoundCtx -> GenericBoundEnv meta builtin
namedBoundContextToEnv ctx = BoundEnv $ do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (bimap (\n -> mkExplicitBinder () (fmap (mempty,) n)) unbound) numberedCtx

boundEnvToCtx :: GenericBoundEnv meta builtin -> NamedBoundCtx
boundEnvToCtx (BoundEnv env) = toNamedBoundCtx (fmap fst env)

-- | Converts an environment to set of values suitable for printing
cheatEnvToValues :: GenericBoundEnv meta builtin -> GenericBoundCtx (GenericForcedValue meta builtin)
cheatEnvToValues (BoundEnv env) = fmap entryToValue env
  where
    entryToValue :: (GenericBinder (), GenericThunk meta builtin) -> GenericForcedValue meta builtin
    entryToValue (binder, value) = do
      let ident = stdlibIdentifier (fromMaybe "_" (nameOf binder) <> " =")
      let arg = explicit value
      VFreeVar ident [arg]

traverseEnv_ ::
  (Monad m) =>
  (GenericThunk meta builtin -> m ()) ->
  GenericBoundEnv meta builtin ->
  m ()
traverseEnv_ f (BoundEnv env) = traverse_ (\(_, v) -> f v) env

traverseEnv ::
  (Monad m) =>
  ( GenericThunk meta builtin ->
    m (GenericThunk meta builtin)
  ) ->
  GenericBoundEnv meta builtin ->
  m (GenericBoundEnv meta builtin)
traverseEnv f (BoundEnv env) = BoundEnv <$> traverse (\(u, v) -> (u,) <$> f v) env

-----------------------------------------------------------------------------
-- Utility functions

boundVariablesIn ::
  forall meta builtin.
  (Show builtin, Show meta) =>
  Lv ->
  GenericThunk meta builtin ->
  Set Lv
boundVariablesIn ctxSize = execWriter . goThunk ctxSize
  where
    goThunk :: (MonadWriter (Set Lv) m) => Lv -> GenericThunk meta builtin -> m ()
    goThunk lv = \case
      Unforced env expr -> goEnvAndExpr lv env expr
      Forced value -> go lv value

    go :: (MonadWriter (Set Lv) m) => Lv -> GenericForcedValue meta builtin -> m ()
    go depth = \case
      VFreeVar _ spine -> goSpine depth spine
      VMeta _ spine -> goSpine depth spine
      VBuiltin _ spine -> goSpine depth spine
      VUniverse {} -> return ()
      VBoundVar lv spine -> do
        when (lv < ctxSize) $
          tell (Set.singleton lv)
        goSpine depth spine
      VPi binder (Closure env bound) -> do
        traverse_ (goThunk depth) binder
        goEnvAndExpr (depth + 1) env bound
      VLam binder (Closure env bound) -> do
        traverse_ (goThunk depth) binder
        goEnvAndExpr (depth + 1) env bound
      VRecord i fs -> do
        goThunk depth i
        traverse_ (goThunk depth) fs
      VRecordAcc _ t _ spine -> do
        goThunk depth t
        goSpine depth spine

    goSpine :: (MonadWriter (Set Lv) m) => Lv -> GenericUnforcedSpine meta builtin -> m ()
    goSpine depth = traverse_ (traverse (goThunk depth))

    goEnvAndExpr :: (MonadWriter (Set Lv) m) => Lv -> GenericBoundEnv meta builtin -> Expr builtin -> m ()
    goEnvAndExpr depth env = traverseBoundVariables_ goLv depth
      where
        goLv newDepth ix = goThunk newDepth (lookupIxInEnv env ix)

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GenericGluedExpr meta builtin = Glued
  { unnormalised :: Expr builtin,
    normalised :: GenericThunk meta builtin
  }
  deriving (Show, Generic)

instance HasProvenance (GenericGluedExpr meta builtin) where
  provenanceOf = provenanceOf . unnormalised

-----------------------------------------------------------------------------
-- Dimensioned values

-- | Because there are no dependent types in Haskell, we cannot create
-- type-classes over tensor values with a given dimension. Hence we need
-- to wrap them in this ugly type-class that stores the dimensions internally.
data DimensionedTensorValue builtin = TensorValue
  { tensorValueDims :: UnforcedDims builtin,
    tensorValue :: Thunk builtin
  }
  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------
-- Instances

instance HasBuiltinConstructor (GenericForcedValue meta) (GenericThunk meta) where
  accessBuiltinC =
    Access
      { getExpr = \case
          VBuiltin b spine -> Just (b, spine)
          _ -> Nothing,
        mkExpr = uncurry VBuiltin
      }
  exprToThunk = Forced

instance HasLambdaConstructor (GenericForcedValue meta) (GenericThunk meta) (GenericClosure meta) where
  accessForcedLamC =
    Access
      { getExpr = \case
          Forced (VLam binder closure) -> Just (binder, closure)
          Unforced env (Lam _p binder body) -> Just (fmap (Unforced env) binder, Closure env body)
          _ -> Nothing,
        mkExpr = \(binder, closure) -> Forced $ VLam binder closure
      }

-----------------------------------------------------------------------------
-- Values for non-type checking passes
-----------------------------------------------------------------------------
-- Outside of type-checking there should be no meta-variables present.

type NoMeta = Void

type ForcedValue = GenericForcedValue NoMeta

type Thunk = GenericThunk NoMeta

type Closure = GenericClosure NoMeta

type BoundEnv = GenericBoundEnv NoMeta

type ForcedType = ForcedValue

type UnforcedType builtin = Thunk builtin

type UnforcedArg builtin = GenericArg (Thunk builtin)

-- | A list of arguments for an application that cannot be normalised.
type UnforcedSpine builtin = [UnforcedArg builtin]

type UnforcedBinder builtin = GenericBinder (Thunk builtin)

type UnforcedTelescope builtin = GenericTelescope (Thunk builtin)

type UnforcedRecordFields builtin = OMap FieldName (Thunk builtin)

type UnforcedDims builtin = Thunk builtin

type GluedExpr builtin = GenericGluedExpr NoMeta builtin

type GluedType builtin = GenericGluedExpr NoMeta builtin

-----------------------------------------------------------------------------
-- Values for type-checking
-----------------------------------------------------------------------------

type ForcedValueWithMetas = GenericForcedValue MetaID

type ThunkWithMetas = GenericThunk MetaID

type ClosureWithMetas = GenericClosure MetaID

type BoundEnvWithMetas = GenericBoundEnv MetaID

type ForcedTypeWithMetas = ForcedValueWithMetas

type UnforcedTypeWithMetas = ThunkWithMetas

type UnforcedBinderWithMetas builtin = GenericUnforcedBinder MetaID builtin

type UnforcedArgWithMetas builtin = GenericUnforcedArg MetaID builtin

type UnforcedSpineWithMetas builtin = [UnforcedArgWithMetas builtin]

type GluedExprWithMetas builtin = GenericGluedExpr MetaID builtin

type GluedTypeWithMetas builtin = GenericGluedExpr MetaID builtin

getNMeta :: GenericForcedValue MetaID builtin -> Maybe MetaID
getNMeta (VMeta m _) = Just m
getNMeta _ = Nothing
