module Vehicle.Compile.Scope.RecordInstances
  ( createAuxilliaryRecordDeclarations,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Core

--------------------------------------------------------------------------------
-- Expr generalisation

createAuxilliaryRecordDeclarations ::
  (MonadScope builtin m) =>
  Provenance ->
  Identifier ->
  Maybe DefRecordSort ->
  Telescope builtin ->
  RecordFields builtin ->
  m [Decl builtin]
createAuxilliaryRecordDeclarations p ident sort telescope fields = do
  let visibility = if isAnnotatedAsTypeClass sort then Instance True else Explicit
  recordProjectionFunctions <- traverse (createRecordProjectionFn p ident telescope visibility) fields
  tensorConversionFunctions <- generateAuxiliaryRecordDefinitions p ident sort telescope fields
  return $ recordProjectionFunctions <> tensorConversionFunctions

-- | Given a record declaration of the form
--
--    def record X t1 .. tn where
--        { ...
--        , f : t
--        , ...
--        }
--
-- creates a projection function:
--
--    f : forall {t1} ... {tn} -> [ X t1 ... tn ] -> t / [t1 ... tn]
--    f {p1} ... {pn} [r] = r.f
--
-- where `[ ... ]` represents the provided visibility.
createRecordProjectionFn ::
  (MonadScope builtin m) =>
  Provenance ->
  Identifier ->
  Telescope builtin ->
  Visibility ->
  RecordField builtin ->
  m (Decl builtin)
createRecordProjectionFn p ident telescope visibility (field, fieldType) = do
  -- Change any explicit binders to implicit
  let implicitTelescope = fmap (flip setBinderVisibility $ Implicit True) telescope

  -- Create the parameters
  let parameterIxs = reverse $ fmap Ix [0 .. (length telescope - 1)]
  let mkParameterArg (binder, ix) = argFromBinder binder (BoundVar p ix)
  let parameterArgs = fmap mkParameterArg (zip telescope parameterIxs)
  let parameterisedRecordType = normAppList (FreeVar p ident) parameterArgs
  let fnRecordBinder namingForm =
        Binder
          { binderDisplayForm = BinderDisplayForm namingForm True,
            binderVisibility = visibility,
            binderRelevance = Relevant,
            binderValue = parameterisedRecordType
          }

  -- Create the type
  let liftedFieldType = liftDBIndices 1 fieldType
  let fnBaseType = Pi p (fnRecordBinder OnlyType) liftedFieldType
  let fnType = foldr (Pi p) fnBaseType implicitTelescope

  -- Create the body
  let liftedRecordType = liftDBIndices 1 parameterisedRecordType
  let recordProjExpr = RecordProj p liftedRecordType (BoundVar p (Ix 0)) field
  let fnBaseBody = Lam p (fnRecordBinder (NameAndType "r" p)) recordProjExpr
  let fnBody = foldr (Lam p) fnBaseBody implicitTelescope

  -- Create the identifier
  let fnIdent = fieldAccessIdentifier ident field
  let fnSort = ProjectionDecl (length telescope + 1)

  -- Create the declaration
  return $ DefFunction p fnIdent fnSort fnType fnBody
