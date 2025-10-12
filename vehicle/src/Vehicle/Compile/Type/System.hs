module Vehicle.Compile.Type.System where

import Data.Hashable (Hashable)
import Data.Proxy (Proxy)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint.Core (instantiateInstanceConstraintSolution)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin)
import Vehicle.Data.Builtin.Standard.Core (Builtin (..))
import Vehicle.Data.Code.Value (Value)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

-- | The type-checking monad.
type TCM builtin m =
  ( MonadTypeChecker builtin m,
    HasTypeSystem builtin
  )

-- | A class that provides an abstract interface for a set of builtins.
class (Eq builtin, Hashable builtin, NormalisableBuiltin builtin, TypableBuiltin builtin) => HasTypeSystem builtin where
  convertFromStandardBuiltins ::
    (MonadTypeChecker builtin m) =>
    Expr Builtin ->
    m (Expr builtin)

  restrictDeclType ::
    (MonadTypeChecker builtin m) =>
    RestrictedDecl ->
    DeclProvenance ->
    Type builtin ->
    m (Type builtin)

  restrictRecordAnnotatedAsTensor ::
    (MonadTypeChecker builtin m) =>
    DeclProvenance ->
    [RecordField (Type builtin)] ->
    m ()

  addAuxiliaryInputOutputConstraints ::
    (MonadTypeChecker builtin m) => Decl builtin -> m (Decl builtin)

  generateDefaultAuxiliaryConstraint ::
    (MonadTypeChecker builtin m) =>
    Proxy builtin ->
    m Bool

  isAuxiliaryConstraint ::
    Expr builtin -> Bool

  -- | Solves an auxiliary instance constraint (i.e. a constraint that is
  -- not solvable by the default instance mechanism)
  solveAuxiliaryInstanceConstraint ::
    (MonadTypeChecker builtin m, MonadFreeContext builtin m) =>
    WithContext (InstanceConstraint builtin) ->
    m ()

-- | Attempts to solve as many type-class constraints as possible.
runAuxiliarySolver :: forall builtin m. (TCM builtin m) => Proxy builtin -> m ()
runAuxiliarySolver proxy = do
  logCompilerSection2 MaxDetail "auxiliary solver run" $
    runConstraintSolver @builtin
      getActiveAuxiliaryInstanceConstraints
      setAuxiliaryInstanceConstraints
      solveAuxiliaryInstanceConstraint
      False
      proxy

-----------------------------------------------------------------------------
-- Standard builtins
-----------------------------------------------------------------------------

extractElementType :: (PrintableBuiltin builtin1, PrintableBuiltin builtin2) => builtin1 -> [Arg builtin2] -> Expr builtin2
extractElementType b args = case args of
  [tElem] -> argExpr tElem
  _ -> monomorphisationError b args

monomorphisationError :: (PrintableBuiltin builtin1, PrintableBuiltin builtin2) => builtin1 -> [Arg builtin2] -> a
monomorphisationError b args = do
  let exprDoc = prettyVerbose args
  developerError $
    "Monomorphisation should have got rid of" <+> quotePretty (show b) <> "s but found applied to args" <+> squotes exprDoc

-----------------------------------------------------------------------------
-- Auxiliary constraint progress

data AuxiliaryConstraintProgress builtin
  = Stuck MetaSet
  | Progress [WithContext (UnificationConstraint builtin)] [WithContext (InstanceConstraint builtin)]
  deriving (Show)

handleAuxiliaryConstraintProgress ::
  (MonadTypeChecker builtin m) =>
  Value builtin ->
  WithContext (InstanceConstraint builtin) ->
  AuxiliaryConstraintProgress builtin ->
  m ()
handleAuxiliaryConstraintProgress solution originalConstraint@(WithContext _ ctx) = \case
  Stuck metas -> addAuxiliaryInstanceConstraints [blockConstraintOn originalConstraint metas]
  Progress newUnificationConstraints newAuxiliaryConstraints -> do
    let normSolution = quote mempty (boundCtxLv $ boundContextOf ctx) solution
    instantiateInstanceConstraintSolution originalConstraint normSolution
    addUnificationConstraints newUnificationConstraints
    addAuxiliaryInstanceConstraints newAuxiliaryConstraints

instance Semigroup (AuxiliaryConstraintProgress builtin) where
  Stuck m1 <> Stuck m2 = Stuck (m1 <> m2)
  Stuck {} <> x@Progress {} = x
  x@Progress {} <> Stuck {} = x
  Progress u1 r1 <> Progress u2 r2 = Progress (u1 <> u2) (r1 <> r2)

blockOn :: (MonadCompile m) => [MetaID] -> Maybe (m (AuxiliaryConstraintProgress builtin))
blockOn metas = Just $ do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas
