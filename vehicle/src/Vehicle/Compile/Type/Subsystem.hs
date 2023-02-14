module Vehicle.Compile.Type.Subsystem
  ( TypableBuiltin (..),
  )
where

import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-- | A class that provides an abstract interface for a set of builtins.
class NormalisableBuiltin builtin => TypableBuiltin builtin where
  -- | Construct a type for the builtin
  typeBuiltin ::
    Provenance -> builtin -> DBExpr builtin

  typeLiteral ::
    Provenance -> Literal -> DBExpr builtin

  typeVectorLiteral ::
    Provenance -> [CheckedType builtin] -> CheckedType builtin

  typeResource ::
    MonadTypeChecker builtin m =>
    Resource ->
    DeclProvenance ->
    GluedType builtin ->
    m (CheckedType builtin)

  isAuxiliaryTypeClassConstraint :: TypeClassConstraint builtin -> Bool

  addAuxiliaryInputOutputConstraints ::
    MonadTypeChecker builtin m => CheckedDecl builtin -> m (CheckedDecl builtin)

  generateDefaultConstraint ::
    MonadTypeChecker builtin m =>
    [WithContext (TypeClassConstraint builtin)] ->
    m (Maybe (WithContext (Constraint builtin)))

  insertHolesForAuxAnnotations ::
    MonadTypeChecker builtin m => UncheckedExpr builtin -> m (UncheckedExpr builtin)

  typeClassRelevancy :: MonadCompile m => builtin -> m Relevance

  -- | Solves a type-class constraint
  solveInstance ::
    (MonadNorm builtin m, MonadTypeChecker builtin m) => WithContext (TypeClassConstraint builtin) -> m ()

  getPropertyInfo :: MonadCompile m => TypedDecl builtin -> m PropertyInfo

  handleTypingError ::
    MonadCompile m => TypingError builtin -> m a
