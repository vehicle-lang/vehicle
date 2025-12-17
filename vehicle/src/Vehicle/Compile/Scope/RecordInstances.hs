module Vehicle.Compile.Scope.RecordInstances
  ( createAuxilliaryRecordDeclarations,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Core
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL

--------------------------------------------------------------------------------
-- Expr generalisation

createAuxilliaryRecordDeclarations ::
  (MonadScope m) =>
  Provenance ->
  Identifier ->
  Maybe DefRecordSort ->
  Telescope Builtin ->
  RecordFields Builtin ->
  m [Decl Builtin]
createAuxilliaryRecordDeclarations p ident sort telescope fields = do
  let visibility = if isAnnotatedAsTypeClass sort then Instance True else Explicit
  recordProjectionFunctions <- traverse (createRecordProjectionFn p ident telescope visibility) fields
  tensorConversionFunctions <-
    if isAnnotatedAsTensor sort
      then createTensorRecordConversionFunctions p ident telescope fields
      else return []
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
  (MonadScope m, PrintableBuiltin builtin) =>
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

createTensorRecordConversionFunctions ::
  (MonadScope m) =>
  Provenance ->
  Identifier ->
  Telescope Builtin ->
  RecordFields Builtin ->
  m [Decl Builtin]
createTensorRecordConversionFunctions p ident telescope fields = do
  unless (null telescope) $
    throwError $
      UnimplementedFeature p ("Annotating parameterised records with" <+> pretty AnnTensor)

  nonEmptyFields <- case fields of
    [] -> throwError $ ZeroFieldTensorLike (ident, p)
    f : fs -> return $ f :| fs

  -- We can't actually know the element and the field types at scope checking
  -- time because the user may be using type synonyms for the tensors, e.g.
  --
  --    @tensor
  --    record Input where
  --      { red   : Image
  --      , green : Image
  --      , blue  : Image
  --      }
  --
  -- but if we make holes for them, the type checker should be able to fill
  -- them in.
  let fieldElementType = hole
  let fieldDimensions = hole

  let recordToTensorDecl = createRecordToTensor p ident fieldElementType fieldDimensions nonEmptyFields
  let tensorToRecordDecl = createTensorToRecord p ident fieldElementType fieldDimensions nonEmptyFields

  return
    [ recordToTensorDecl,
      tensorToRecordDecl
    ]

{-

toRecord : Tensor A (2 : _) -> r
toRecord t =
  { f1 = t ! 0
  , f2 = t ! 1
  }

-}

createRecordToTensor ::
  Provenance ->
  Identifier ->
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  NonEmpty (GenericRecordField (Type Builtin)) ->
  Decl Builtin
createRecordToTensor p recordIdent fieldElementType fieldDimensions fields = do
  -- Create the name
  let functionName = Text.pack "_" <> nameOf recordIdent <> "ToTensor"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  -- Create the type
  let firstDimension = dim (length fields)
  let allDimensions = dimCons firstDimension fieldDimensions
  let recordType = freeVar recordIdent
  let functionType = fromDSL mempty $ recordType ~> tTensor fieldElementType allDimensions

  -- Create the body
  let functionBody = fromDSL mempty $ explLam "x" recordType $ \r -> do
        let tensorElements = fmap (\(fieldName, _) -> recordProj (freeVar recordIdent) r fieldName) fields
        stackTensor fieldElementType firstDimension fieldDimensions tensorElements

  DefFunction p functionIdent (FunctionDecl 1 Nothing) functionType functionBody

createTensorToRecord ::
  Provenance ->
  Identifier ->
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  NonEmpty (GenericRecordField (Type Builtin)) ->
  Decl Builtin
createTensorToRecord p recordIdent fieldElementType fieldDimensions fields = do
  -- Create the name
  let functionName = Text.pack "_" <> nameOf recordIdent <> "FromTensor"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  -- Create the type
  let firstDimension = dim (length fields)
  let recordType = freeVar recordIdent
  let tensorType = tTensor fieldElementType (dimCons firstDimension fieldDimensions)
  let functionType = fromDSL mempty $ tensorType ~> recordType

  let fieldNames = fmap fst (toList fields)
  let tensorIndices = fmap indexLit ([0 .. length fields - 1] :: [Int]) :: [DSLExpr Builtin]

  -- Create the body
  let functionBody = fromDSL mempty $ explLam "x" tensorType $ \tensor -> do
        let fieldContents = fmap (\index -> atTensor fieldElementType firstDimension fieldDimensions tensor index) tensorIndices
        record recordType (zip fieldNames fieldContents)

  DefFunction p functionIdent (FunctionDecl 1 Nothing) functionType functionBody
