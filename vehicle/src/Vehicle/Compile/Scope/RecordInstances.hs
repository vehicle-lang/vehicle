module Vehicle.Compile.Scope.RecordInstances
  ( createTensorRecordConversionFunctions,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Core
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL

--------------------------------------------------------------------------------
-- Expr generalisation

createTensorRecordConversionFunctions ::
  (MonadScope m) =>
  Provenance ->
  Identifier ->
  [RecordField (Type Builtin)] ->
  m [Decl Builtin]
createTensorRecordConversionFunctions p ident fields = do
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
  return
    [ recordToTensorDecl
    ]

createRecordToTensor ::
  Provenance ->
  Identifier ->
  DSLExpr Builtin ->
  DSLExpr Builtin ->
  NonEmpty (RecordField (Type Builtin)) ->
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
  let functionBody = fromDSL mempty $ explLam "x" recordType $ \record -> do
        let tensorElements = fmap (\(fieldName, _) -> recordAcc record (recordIdent, fieldName)) fields
        stackTensor fieldElementType firstDimension fieldDimensions tensorElements

  DefFunction p functionIdent mempty functionType functionBody
