{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Scope.Records where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable (forM)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface (getDims)
import Vehicle.Data.Code.TypedView (TypeValue (VRatTensorType), toTypeValue)
import Vehicle.Data.Code.Value
import Vehicle.Data.DSL
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Libraries.StandardLibrary
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Expr generalisation

generateBuiltinAuxiliaryRecordDefinitions ::
  (MonadCompile m) =>
  Provenance ->
  Identifier ->
  Maybe DefRecordSort ->
  Telescope Builtin ->
  RecordFields Builtin ->
  [DerivableRecordOperation] ->
  m [Decl Builtin]
generateBuiltinAuxiliaryRecordDefinitions p ident sort telescope fields derivedOps = do
  -- Create the projection functions for the record
  let visibility = if isAnnotatedAsTypeClass sort then Instance True else Explicit
  recordProjectionFunctions <- traverse (createRecordProjectionFn p ident telescope visibility) fields

  -- All records can be used as inputs and outputs to networks
  -- so generate the marker instances that allow them to pass type-checking.
  let validNetworkIOInstances
        | isStandardLibIdent ident = []
        | otherwise = [createRecordHasValidIOTypeInstance p ident telescope fields]

  -- Generate the instances to support comparison between records
  let recordComparisonInstances = [createRecordComparisonInstance p ident telescope fields]

  -- Generate the conversion  tensor conversion functions
  tensorConversionFunctionsAndInstances <-
    if isAnnotatedAsTensor sort
      then createTensorRecordConversionFunctions p ident telescope fields
      else return []

  -- Generate the instances for the supports
  derivedInstances <- generateDerivedInstances p ident telescope fields derivedOps

  return $
    recordProjectionFunctions
      <> validNetworkIOInstances
      <> recordComparisonInstances
      <> tensorConversionFunctionsAndInstances
      <> derivedInstances

--------------------------------------------------------------------------------
-- Record projections
--------------------------------------------------------------------------------

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
  (MonadLogger m) =>
  Provenance ->
  Identifier ->
  Telescope Builtin ->
  Visibility ->
  RecordField Builtin ->
  m (Decl Builtin)
createRecordProjectionFn p ident telescope visibility (field, fieldType) = do
  -- Change any explicit binders to implicit and create parameters
  let parameterArgs = createArgsForTelescope p telescope
  let parameterisedRecordType = normAppList (FreeVar p ident) parameterArgs
  let fnRecordBinder namingForm =
        Binder
          { binderDisplayForm = BinderDisplayForm namingForm True,
            binderVisibility = visibility,
            binderRelevance = Relevant,
            binderValue = parameterisedRecordType
          }

  -- Create the type
  let implicitTelescope = createImplicitTelescope telescope
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

--------------------------------------------------------------------------------
-- ValidNetworkIOType instance generation
--------------------------------------------------------------------------------

instanceBinder :: Provenance -> Name -> Expr Builtin -> GenericBinder (Expr Builtin)
instanceBinder p name = Binder (BinderDisplayForm (NameAndType name p) True) (Instance True) Relevant

createRecordHasValidIOTypeInstance ::
  Provenance ->
  Identifier ->
  Telescope Builtin ->
  RecordFields Builtin ->
  Decl Builtin
createRecordHasValidIOTypeInstance p recordIdent telescope fields = do
  -- For each record R we want to create a function that looks like:
  --
  --   @instance
  --   recordRHasValidNetworkIOType :
  --     {t1} ->
  --     ...
  --     {tn} ->
  --     {{HasValidNetworkFieldType f1}} ->
  --     ...
  --     {{HasValidNetworkFieldType fn}} ->
  --     HasValidNetworkIOType (R t1 ... tn)
  --   recordRHasValidNetworkIOType t1 ... tn = {}
  --
  -- ... where t1 through tn are the types in the telescope,
  -- and f1 through fn are the types of R's fields.

  -- Create the name
  let instanceName = Text.pack "record" <> nameOf recordIdent <> "HasValidNetworkIOType"
  let instanceIdent = Identifier (modulePath recordIdent) instanceName

  let mkConstraint (FieldName pf n, fieldType) k = instanceBinder pf n $ normAppList target [argument]
        where
          target = FreeVar mempty validNetworkFieldTypeIdent
          argument = explicit (liftDBIndices (Lv k) fieldType)

  -- Construct both the telescope and the typeclass constraints
  let implicitTelescope = fmap (flip setBinderVisibility $ Implicit True) telescope
  let constraintBinders = zipWith mkConstraint fields [0 .. length fields]
  let binderList = implicitTelescope ++ constraintBinders

  -- Create the type arguments for the fully applied record type
  let mkArg (binder, ix) = argFromBinder binder (BoundVar p ix)
  let binderIndices = reverse $ fmap Ix [0 .. (length binderList - 1)]
  let args = fmap mkArg (zip telescope binderIndices)

  -- Create the applied record and result type
  let parameterisedRecordType = toDSL $ normAppList (FreeVar p recordIdent) args
  let resultType = fromDSL mempty $ freeVar validNetworkIOTypeIdent @@ [parameterisedRecordType]

  let instanceMethods = []

  -- Create the function
  let functionType = foldr (Pi p) resultType binderList
  let functionBody = foldr (Lam p) (Record p resultType instanceMethods) binderList
  let functionSort = FunctionDecl 1 (Just (AnnInstance Nothing))

  DefFunction p instanceIdent functionSort functionType functionBody

createRecordComparisonInstance ::
  Provenance ->
  Identifier ->
  Telescope Builtin ->
  RecordFields Builtin ->
  Decl Builtin
createRecordComparisonInstance p recordIdent telescope fields = do
  --
  -- For a record like:
  --
  --    record Pair t1 t2
  --      { f1 : t1
  --      , f2 : t2
  --      }
  --
  -- We need to generate methods of the form:
  --
  --    @instance
  --    recordPairHasComparisonInstance :
  --      T1 -> T2 ->
  --      HasComparisonInstance T1 T1 -> HasComparisonInstance T2 T2 ->
  --      HasComparison (Pair T1 T2) (Pair T2 T1)
  --    recordPairHasComparisonInstance T1 T2 compT1 compT2 =
  --      { method = \r1 r2 -> (compT1.method r1.f1 r2.f1) and (compT2.method r1.f2 r2.f2)
  --      , ...
  --      }
  --
  -- ... with methods for each comparison op (<, <=, >, >=, ==, !=)
  --

  let instanceName = Text.pack "record" <> nameOf recordIdent <> "HasComparison"
  let instanceIdent = Identifier (modulePath recordIdent) instanceName

  let mkConstraint (FieldName _ n, fieldType) k = instanceBinder mempty (n <> "Comparison") $ normAppList target [argument, argument]
        where
          target = FreeVar mempty hasComparisonIdent
          argument = explicit (liftDBIndices (Lv k) fieldType)

  let implicitTelescope = fmap (flip setBinderVisibility $ Implicit True) telescope
  let constraintBinders = zipWith mkConstraint fields [0 .. length fields]
  let binderList = implicitTelescope ++ constraintBinders

  let mkArg (binder, ix) = argFromBinder binder (BoundVar p ix)
  let binderIndices = reverse $ fmap Ix [0 .. (length binderList - 1)]
  let args = fmap mkArg (zip telescope binderIndices)

  let parameterisedRecordType = toDSL $ normAppList (FreeVar p recordIdent) args
  let resultType = fromDSL mempty $ freeVar hasComparisonIdent @@ [parameterisedRecordType, parameterisedRecordType]

  let for = flip map
  let for2 a b f = zipWith f a b
  let finalRecordType = fromDSL mempty parameterisedRecordType

  let instanceMethods = for ["leTC", "ltTC", "geTC", "gtTC", "eqTC", "neTC"] $ \methodName -> do
        -- Create two new binders, for the records that we are testing and use them to recalculate our indices:
        --    \(r1 : Pair T1 T2) (r2 : Pair T1 T2) -> <body>
        let lhsRecord = mkExplicitBinder (liftDBIndices (Lv 0) finalRecordType) (Just (mempty, "r1"))
        let rhsRecord = mkExplicitBinder (liftDBIndices (Lv 1) finalRecordType) (Just (mempty, "r2"))
        let newBinderList = binderList ++ [lhsRecord, rhsRecord]
        let newBinderIndicies = reverse $ fmap Ix [0 .. length newBinderList - 1]

        -- Generate all the comparions operations that need to take place for our method:
        --    [ compT1.method r1.f1 r2.f2
        --    , compT2.method r1.f2 r2.f2
        --    ]
        let comparisonIndicies = take (length fields) $ drop (length telescope) newBinderIndicies
        let individualComparisons = for2 fields comparisonIndicies $ \(fieldName, fieldTy) ix -> do
              -- Project out the comparison method from the instance
              let liftedFieldType = liftDBIndices (Lv $ length newBinderList - length telescope) fieldTy
              let comparisonInstanceType = normAppList (FreeVar mempty hasComparisonIdent) [explicit liftedFieldType, explicit liftedFieldType]
              let comparisonMethod = RecordProj mempty comparisonInstanceType (BoundVar mempty ix) (FieldName mempty methodName)

              -- Project out the fields from the records
              let liftedRecordType = liftDBIndices (Lv 2) finalRecordType
              let lhsRecordField = RecordProj mempty liftedRecordType (BoundVar mempty 1) fieldName
              let rhsRecordField = RecordProj mempty liftedRecordType (BoundVar mempty 0) fieldName

              -- Apply the method to the fields
              normAppList comparisonMethod [explicit lhsRecordField, explicit rhsRecordField]

        -- Join all of our comparisons together with logical 'and' then use the resulting expression as our method body:
        --    \(r1 : Pair T1 T2) (r2 : Pair T1 T2) -> (compT1.method r1.f1 r2.f1) and (compT2.method r1.f2 r2.f2)
        let and' l r = normAppList (Builtin mempty $ BuiltinFunction And) [explicit l, explicit r]
        let methodFn = Lam mempty lhsRecord $ Lam mempty rhsRecord $ foldr1 and' individualComparisons
        (FieldName mempty methodName, methodFn)

  let functionType = foldr (Pi p) resultType binderList
  let functionBody = foldr (Lam p) (Record p resultType instanceMethods) binderList
  let functionSort = FunctionDecl 1 (Just (AnnInstance Nothing))
  DefFunction p instanceIdent functionSort functionType functionBody

--------------------------------------------------------------------------------
-- @tensor annotations
--------------------------------------------------------------------------------

createTensorRecordConversionFunctions ::
  (MonadCompile m) =>
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
  --    type Image = Tensor Real [28, 28]
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
  let validNetworkFieldInstance = createValidNetworkFieldInstance p ident
  let validDatasetTypeInstance = createValidDatasetTypeInstance p ident
  let validDatasetListElementTypeInstance = createValidDatasetListElementTypeInstance p ident
  let quantInstance = createTensorLikeHasQuantifierInstance p ident

  let instances =
        [ recordToTensorDecl,
          tensorToRecordDecl,
          validNetworkFieldInstance,
          validDatasetTypeInstance,
          validDatasetListElementTypeInstance,
          quantInstance
        ]

  -- All @tensor annotations should also derive the following operations
  return instances

createValidNetworkFieldInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createValidNetworkFieldInstance p recordIdent = do
  let recordType = fromDSL mempty $ freeVar validNetworkFieldTypeIdent @@ [freeVar recordIdent]
  let functionBody = Record p recordType []

  let functionName = Text.pack "_" <> nameOf recordIdent <> "HasValidNetworkFieldType"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody

createValidDatasetTypeInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createValidDatasetTypeInstance p recordIdent = do
  let recordType = fromDSL mempty $ freeVar validDatasetTypeIdent @@ [freeVar recordIdent]
  let functionBody = Record p recordType []

  let functionName = Text.pack "_" <> nameOf recordIdent <> "HasValidDatasetType"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody

createValidDatasetListElementTypeInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createValidDatasetListElementTypeInstance p recordIdent = do
  let recordType = fromDSL mempty $ freeVar validDatasetListElementTypeIdent @@ [freeVar recordIdent]
  let functionBody = Record p recordType []

  let functionName = Text.pack "_" <> nameOf recordIdent <> "HasValidDatasetListElementType"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody

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

  DefFunction p functionIdent (TensorCoercionDecl 1) functionType functionBody

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

  DefFunction p functionIdent (TensorCoercionDecl 1) functionType functionBody

createTensorLikeHasQuantifierInstance ::
  Provenance ->
  Identifier ->
  Decl Builtin
createTensorLikeHasQuantifierInstance p recordIdent = do
  let recordType = fromDSL mempty $ freeVar hasQuantifierIdent @@ [freeVar recordIdent]

  let functionName = Text.pack "_" <> nameOf recordIdent <> "HasQuantifier"
  let functionIdent = Identifier (modulePath recordIdent) functionName

  let forAllTCFieldName = FieldName p "forallTC"
  let existsTCFieldName = FieldName p "existsTC"

  let functionBody =
        Record
          p
          recordType
          [ (forAllTCFieldName, fromDSL mempty (builtinFunction (QuantifyRecord Forall))),
            (existsTCFieldName, fromDSL mempty (builtinFunction (QuantifyRecord Exists)))
          ]

  DefFunction p functionIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) recordType functionBody

--------------------------------------------------------------------------------
-- Derivable operations
--------------------------------------------------------------------------------

generateDerivedInstances ::
  (MonadLogger m) =>
  Provenance ->
  Identifier ->
  Telescope Builtin ->
  RecordFields Builtin ->
  [DerivableRecordOperation] ->
  m [Decl Builtin]
generateDerivedInstances p ident telescope fields ops = do
  let uniqueOps = Set.toList $ Set.fromList ops
  derivedDecls <- forM uniqueOps $ \case
    Addition ->
      return
        [ deriveArithmeticOp2 hasAddIdent addTCProj p ident telescope fields,
          deriveArithmeticOp2 hasSubIdent subTCProj p ident telescope fields
        ]
    Multiplication ->
      return
        [ deriveArithmeticOp2 hasMulIdent mulTCProj p ident telescope fields,
          deriveArithmeticOp2 hasDivIdent divTCProj p ident telescope fields
        ]

  return $ concat derivedDecls

deriveArithmeticOp2 ::
  Identifier -> -- standard library identifier for the typeclass, e.g. hasAddIdent
  Identifier -> -- name of the field for the operation in text e.g. addTC
  Provenance ->
  Identifier ->
  Telescope Builtin ->
  RecordFields Builtin ->
  Decl Builtin
deriveArithmeticOp2 typeclassIdent typeclassOp p recordIdent telescope fields = do
  -- Create the name of the instance
  let instanceName = Text.pack "_" <> nameOf recordIdent <> nameOf typeclassIdent
  let instanceIdent = Identifier (modulePath recordIdent) instanceName

  -- Helper function to make the record type.
  -- TODO: generalise `@@` etc. to take lists instead of non-empty lists.
  let mkRecordType = \case
        [] -> freeVar recordIdent
        (a : as) -> freeVar recordIdent @@ (a :| as)
  let mkInstanceType args = do
        let recordType = mkRecordType args
        freeVar typeclassIdent @@ [recordType, recordType, recordType]

  -- Create the applied type of the record
  let implicitTelescope = createImplicitTelescope telescope
  let instanceType = fromDSL p $
        forallTelescope implicitTelescope $ \args -> do
          mkInstanceType args

  -- Create the body of the type
  let instanceBody = fromDSL p $
        forallTelescope implicitTelescope $ \args -> do
          let recordType = mkRecordType args
          let operation =
                explLam "x" recordType $ \x ->
                  explLam "y" recordType $ \y -> do
                    let mkField (fieldName, _fieldType) =
                          ( fieldName,
                            freeVar typeclassOp
                              @@ [ recordProj recordType x fieldName,
                                   recordProj recordType y fieldName
                                 ]
                          )
                    record recordType $ fmap mkField fields

          record (mkInstanceType args) [(FieldName p (nameOf typeclassOp), operation)]

  DefFunction p instanceIdent (FunctionDecl 1 (Just (AnnInstance Nothing))) instanceType instanceBody

-- -----------------------------------------------------------------------------------------------
-- Record/Tensorisable util functions
-- Not sure if these should go here or if they are at the right level of abstraction

constructTensorisableDims ::
  GenericRecordFields (Value Builtin) ->
  TensorShape
constructTensorisableDims [] = []
constructTensorisableDims fields@((_n, typ) : _fs) =
  case toTypeValue typ of
    (VRatTensorType dims) ->
      case getDims dims of
        Just d -> length fields : d
        Nothing -> [length fields]
    _ -> [length fields]

constructFromTensorFreeVar ::
  Identifier ->
  Provenance ->
  Expr Builtin
constructFromTensorFreeVar ident p = do
  let name = Text.pack "_" <> identifierName ident <> "FromTensor"
  FreeVar p (Identifier (modulePath ident) name)

constructToTensorFreeVar ::
  Identifier ->
  Provenance ->
  Expr Builtin
constructToTensorFreeVar ident p = do
  let name = Text.pack "_" <> identifierName ident <> "ToTensor"
  FreeVar p (Identifier (modulePath ident) name)

createImplicitTelescope :: Telescope Builtin -> Telescope Builtin
createImplicitTelescope = fmap (flip setBinderVisibility $ Implicit True)

createArgsForTelescope :: Provenance -> Telescope Builtin -> [Arg Builtin]
createArgsForTelescope p telescope = do
  let mkArg (binder, ix) = argFromBinder binder (BoundVar p ix)
  let binderIndices = reverse $ fmap Ix [0 .. (length telescope - 1)]
  fmap mkArg (zip telescope binderIndices)

forallTelescope :: Telescope Builtin -> ([DSLExpr Builtin] -> DSLExpr Builtin) -> DSLExpr Builtin
forallTelescope telescope continuation = go [] telescope
  where
    go :: [DSLExpr Builtin] -> Telescope Builtin -> DSLExpr Builtin
    go args = \case
      [] -> continuation (reverse args)
      b : bs -> pi (nameOf b) (visibilityOf b) (relevanceOf b) hole $ \a -> go (a : args) bs
