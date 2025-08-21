module Vehicle.Backend.Agda.CapitaliseTypeNames
  ( capitaliseTypeNames,
  )
where

import Control.Monad (when)
import Control.Monad.State (MonadState (..), evalStateT, modify)
import Data.Data (Proxy (..))
import Data.Set (Set, insert, member)
import Vehicle.Compile.Context.Free (MonadFreeContext, addDeclToContext, runFreshFreeContextT)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (normaliseClosure, normaliseInEmptyEnv)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Decidability (DecidabilityBuiltin (..), DecidabilityBuiltinFunction (..))
import Vehicle.Data.Code.Value (Value (..))

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

      d' <- traverseDeclTypeAndExpr cap cap d
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
    Record p ident fields -> do
      ident' <- capitaliseIdentifierIfType ident
      Record p ident' <$> traverseRecordFields cap fields
    RecordAcc p record (ident, field) -> do
      ident' <- capitaliseIdentifierIfType ident
      RecordAcc p <$> cap record <*> pure (ident', field)

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

isTypeDef :: forall m. (MonadCapitalise m) => Decl DecidabilityBuiltin -> m Bool
isTypeDef decl = do
  normType <- normaliseInEmptyEnv (typeOf decl)
  case normType of
    -- We don't capitalise things of type `Bool` because they will be lifted
    -- to the type level, only things of type `X -> Bool`.
    VPi {} -> go mempty normType
    _ -> return False
  where
    go :: NamedBoundCtx -> Value DecidabilityBuiltin -> m Bool
    go _ (VBuiltin (DecidabilityBuiltinFunction PropType) []) = return True
    go ctx (VPi binder closure) = do
      result <- normaliseClosure ctx binder closure
      go (nameOf binder : ctx) result
    go _ _ = return False
