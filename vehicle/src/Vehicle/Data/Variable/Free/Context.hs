module Vehicle.Data.Variable.Free.Context
  ( module X,
    addDeclToContext,
    traverseDeclsInCtx_,
    getRecordFields,
    getRecordProvenance,
    getRecordFieldNames,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Variable.Free.Context.Class as X
import Vehicle.Data.Variable.Free.Context.Core as X
import Vehicle.Data.Variable.Free.Context.Instance as X

addDeclToContext ::
  (MonadFreeContext builtin m) =>
  Decl builtin ->
  m a ->
  m a
addDeclToContext = addDeclEntryToContext

traverseDeclsInCtx_ ::
  forall m builtin.
  (MonadLogger m, PrintableBuiltin builtin) =>
  (Decl builtin -> FreeContextT builtin m ()) ->
  Prog builtin ->
  m ()
traverseDeclsInCtx_ f (Main ds) =
  runFreshFreeContextT (Proxy @builtin) $ do
    go ds
  where
    go :: [Decl builtin] -> FreeContextT builtin m ()
    go = \case
      [] -> return ()
      decl : decls -> do
        _ <- f decl
        decls' <- addDeclEntryToContext decl $ go decls
        return decls'

getRecordFields ::
  (MonadFreeContext Builtin m) =>
  Identifier ->
  m (GenericRecordFields (Expr Builtin))
getRecordFields ident = do
  decl <- getDeclEntry (Proxy @Builtin) ident
  case decl of
    DefRecord _ _ _ _ fields _ -> return fields
    _ -> developerError "Record declaration is not of expected format."

getRecordFieldNames ::
  (MonadFreeContext Builtin m) =>
  Identifier ->
  m (NonEmpty Name)
getRecordFieldNames ident = do
  decl <- getDeclEntry (Proxy @Builtin) ident
  fieldNames <- case decl of
    DefRecord _p _ident _sort _telescope fields _supportedOps -> return $ map (\(field, _typ) -> nameOf field) fields
    _ -> developerError "Record declaration is not of expected format."

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
    (DefRecord p _ _ _ _ _) -> return p
    _ -> developerError "Record declaration is not of expected format."
