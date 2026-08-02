module Vehicle.Compile.CapitaliseTypeNames
  ( capitaliseTypeNames,
  )
where

import Control.Monad (when)
import Control.Monad.State (MonadState (..), evalStateT, modify)
import Data.Data (Proxy (..))
import Data.Set (Set, insert, member)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.Force (forceThunk)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Decidability (DecidabilityBuiltin (..), DecidabilityBuiltinFunction (..))
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, runFreshFreeContextT)

--------------------------------------------------------------------------------
-- Capitalise type names

-- | In Agda types (i.e. functions whose result type is `Set`) are capitalised by
-- convention. This pass identifies all such defined functions and capitalises
-- all references to them. Cannot be done during the main compilation pass as we
-- need to be able to distinguish between free and bound variables.
capitaliseTypeNames :: (MonadCompile m) => Prog DecidabilityBuiltin -> m (Prog DecidabilityBuiltin)
capitaliseTypeNames prog = runFreshFreeContextT (Proxy @DecidabilityBuiltin) $ evalStateT (cap prog) mempty

--------------------------------------------------------------------------------
-- Algorithm

type MonadCapitalise m =
  ( MonadCompile m,
    MonadState (Set Identifier) m,
    MonadFreeContext DecidabilityBuiltin m
  )

class CapitaliseTypes a where
  cap :: (MonadCapitalise m) => a -> m a

instance CapitaliseTypes (Prog DecidabilityBuiltin) where
  cap (Main ds) = Main <$> cap ds

instance CapitaliseTypes [Decl DecidabilityBuiltin] where
  cap = \case
    [] -> return []
    d : ds -> do
      isType <- isTypeDef d
      when isType $
        modify (insert (identifierOf d))

      d' <- traverse cap d
      let d'' = if isType then mapIdentifier capitaliseIdentifier d' else d'

      ds' <- addDeclToContext d'' (cap ds)
      return $ d'' : ds'

instance CapitaliseTypes (Expr DecidabilityBuiltin) where
  cap = \case
    Universe p l -> return $ Universe p l
    Hole p n -> return $ Hole p n
    Meta p m -> return $ Meta p m
    Builtin p op -> return $ Builtin p op
    App fun args -> App <$> cap fun <*> traverse cap args
    Pi p binder result -> Pi p <$> cap binder <*> cap result
    Let p bound binder body -> Let p <$> cap bound <*> cap binder <*> cap body
    Lam p binder body -> Lam p <$> cap binder <*> cap body
    BoundVar p v -> return $ BoundVar p v
    FreeVar p ident -> FreeVar p <$> capitaliseIdentifierIfType ident
    Record p recordType fields -> do
      Record p <$> cap recordType <*> traverseRecordFields cap fields
    RecordProj p recordType record field -> do
      RecordProj p <$> cap recordType <*> cap record <*> pure field

instance CapitaliseTypes (Arg DecidabilityBuiltin) where
  cap Arg {..} = do
    argExpr' <- cap argExpr
    return $ Arg {argExpr = argExpr', ..}

instance CapitaliseTypes (Binder DecidabilityBuiltin) where
  cap Binder {..} = do
    binderValue' <- cap binderValue
    return $ Binder {binderValue = binderValue', ..}

capitaliseIdentifier :: Identifier -> Identifier
capitaliseIdentifier (Identifier m s) = Identifier m $ capitaliseFirstLetter s

capitaliseIdentifierIfType :: (MonadCapitalise m) => Identifier -> m Identifier
capitaliseIdentifierIfType ident = do
  typeIdentifiers <- get
  return $
    if member ident typeIdentifiers
      then capitaliseIdentifier ident
      else ident

isTypeDef :: (MonadCapitalise m) => Decl DecidabilityBuiltin -> m Bool
isTypeDef decl = case decl of
  DefAbstract {} -> return False
  DefRecord {} -> return False
  DefFunction _ _ _ t _ -> runFreshNameBoundContextT $ do
    forcedType <- forceThunk $ Unforced emptyBoundEnv t
    case forcedType of
      -- We don't capitalise things of type `Bool` because they will be lifted
      -- to the type level, only things of type `X -> Bool`.
      VPi {} -> go $ Forced forcedType
      _ -> return False
  where
    go ::
      (MonadCapitalise m, MonadNameContext m) =>
      Thunk DecidabilityBuiltin ->
      m Bool
    go t = do
      forcedType <- forceThunk t
      case forcedType of
        (VBuiltin (DecidabilityBuiltinFunction PropType) []) ->
          return True
        (VPi binder closure) -> do
          lv <- getBinderDepth
          let result = extendClosureWithBound closure binder lv
          addNameToContext binder $ go result
        _ -> return False
