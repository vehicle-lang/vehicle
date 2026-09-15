module Vehicle.Data.Variable.Free.Context
  ( module X,
    addDeclToContext,
    getRecordFields,
    getRecordProvenance,
    getRecordFieldNames,
    traverseProgDecls,
    isFunctionWhoseReturnType,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Normalise.Core (NormalisableBuiltin)
import Vehicle.Compile.Normalise.Force (forceThunk)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Name (NameBoundContextT, runFreshNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Class (MonadNameContext (..), getBinderDepth)
import Vehicle.Data.Variable.Free.Context.Class as X
import Vehicle.Data.Variable.Free.Context.Core as X
import Vehicle.Data.Variable.Free.Context.Instance as X

addDeclToContext ::
  (MonadLogger m, MonadFreeContext builtin m) =>
  Decl builtin ->
  m a ->
  m a
addDeclToContext = addDeclEntryToContext

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
  let fields = getRecordFieldsFromDecl decl

  case NonEmpty.nonEmpty fields of
    Just nonEmptyFields -> return $ fmap (\(field, _typ) -> nameOf field) nonEmptyFields
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

traverseProgDecls ::
  forall builtin1 builtin2 m.
  (MonadLogger m, PrintableBuiltin builtin1) =>
  Prog builtin1 ->
  (Decl builtin1 -> FreeContextT builtin1 m (Maybe (Decl builtin2))) ->
  m (Prog builtin2)
traverseProgDecls (Main decls) f = do
  us <- runFreshFreeContextT (Proxy @builtin1) $ go decls
  return $ Main us
  where
    go :: [Decl builtin1] -> FreeContextT builtin1 m [Decl builtin2]
    go = \case
      [] -> return []
      d : ds -> do
        d' <- f d
        ds' <- addDeclToContext d $ go ds
        return $ maybe ds' (: ds') d'

-- | Is the provided declaration a function whose return type matches the
-- provided predicate?
isFunctionWhoseReturnType ::
  forall builtin m.
  (MonadFreeContext builtin m, NormalisableBuiltin builtin) =>
  (ForcedValue builtin -> Bool) ->
  Decl builtin ->
  m Bool
isFunctionWhoseReturnType predicate decl = case decl of
  DefAbstract {} -> return False
  DefRecord {} -> return False
  DefFunction _ _ _ t _ -> runFreshNameBoundContextT $ go $ Unforced emptyBoundEnv t
  where
    go :: Thunk builtin -> NameBoundContextT m Bool
    go t = do
      forcedType <- forceThunk t
      case forcedType of
        VPi binder closure -> do
          lv <- getBinderDepth
          let result = extendClosureWithBound closure binder lv
          addNameToContext binder $ go result
        other -> return $ predicate other
