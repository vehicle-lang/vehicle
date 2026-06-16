module Vehicle.Data.Variable.Free.Context
  ( module X,
    addDeclToContext,
    traverseNormalisedDecls_,
    getRecordFields,
    getRecordFieldNames,
    getRecordProvenance,
    getRecordFieldNamesNE,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Free.Context.Class as X
import Vehicle.Data.Variable.Free.Context.Core as X
import Vehicle.Data.Variable.Free.Context.Instance as X

addDeclToContext ::
  (MonadLogger m, MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  Decl builtin ->
  m a ->
  m a
addDeclToContext decl cont = do
  declEntry <- evalDecl decl
  addDeclEntryToContext declEntry cont

traverseNormalisedDecls_ ::
  forall m builtin.
  (MonadLogger m, NormalisableBuiltin builtin) =>
  (VDecl builtin -> FreeContextT builtin m ()) ->
  Prog builtin ->
  m ()
traverseNormalisedDecls_ f (Main ds) =
  runFreshFreeContextT (Proxy @builtin) $ do
    go ds
  where
    go :: [Decl builtin] -> FreeContextT builtin m ()
    go = \case
      [] -> return ()
      decl : decls -> do
        normDecl <- evalDecl decl
        _ <- f normDecl
        decls' <- addDeclEntryToContext normDecl $ go decls
        return decls'

getRecordFields ::
  (MonadFreeContext Builtin m) =>
  Identifier ->
  m (GenericRecordFields (Value Builtin))
getRecordFields ident = do
  decl <- getDeclEntry (Proxy @Builtin) ident
  case decl of
    DefRecord _ _ _ _ fields -> return fields
    _ -> developerError "Record declaration is not of expected format."

getRecordFieldNames ::
  (MonadFreeContext Builtin m) =>
  Identifier ->
  m [Name]
getRecordFieldNames ident = do
  decl <- getDeclEntry (Proxy @Builtin) ident
  case decl of
    DefRecord _p _ident _sort _telescope fields -> return $ map (\(field, _typ) -> nameOf field) fields
    _ -> developerError "Record declaration is not of expected format."

getRecordFieldNamesNE ::
  (MonadFreeContext Builtin m) =>
  Identifier ->
  m (NonEmpty Name)
getRecordFieldNamesNE ident = do
  fieldNames <- getRecordFieldNames ident
  case NonEmpty.nonEmpty fieldNames of
    Just fields -> pure fields
    Nothing -> developerError "Record contains no fields when fields are expected"

getRecordProvenance ::
  (MonadFreeContext Builtin m) =>
  Identifier ->
  m Provenance
getRecordProvenance ident = do
  decl <- getDeclEntry (Proxy @Builtin) ident
  case decl of
    (DefRecord p _ _ _ _) -> return p
    _ -> developerError "Record declaration is not of expected format."
