module Vehicle.Backend.Agda.CapitaliseTypeNames
  ( capitaliseTypeNames,
  )
where

import Control.Monad (when)
import Control.Monad.State (MonadState (..), evalState, modify)
import Data.Set (Set, insert, member)
import Vehicle.Compile.Prelude
import Vehicle.Expr.BuiltinInterface

--------------------------------------------------------------------------------
-- Capitalise type names

-- | In Agda types (i.e. functions whose result type is `Set`) are capitalised by
-- convention. This pass identifies all such defined functions and capitalises
-- all references to them. Cannot be done during the main compilation pass as we
-- need to be able to distinguish between free and bound variables.
capitaliseTypeNames :: (HasStandardTypes builtin) => Prog var builtin -> Prog var builtin
capitaliseTypeNames prog = evalState (cap prog) mempty

--------------------------------------------------------------------------------
-- Algorithm

type MonadCapitalise m = MonadState (Set Identifier) m

class CapitaliseTypes a where
  cap :: (MonadCapitalise m) => a -> m a

instance (HasStandardTypes builtin) => CapitaliseTypes (Prog var builtin) where
  cap (Main ds) = Main <$> traverse cap ds

instance (HasStandardTypes builtin) => CapitaliseTypes (Decl var builtin) where
  cap d = case d of
    DefAbstract p ident r t ->
      DefAbstract p <$> capitaliseIdentifier ident <*> pure r <*> cap t
    DefFunction p ident anns t e -> do
      when (isTypeDef t) $
        modify (insert ident)
      DefFunction p <$> capitaliseIdentifier ident <*> pure anns <*> cap t <*> cap e

instance CapitaliseTypes (Expr var builtin) where
  cap = \case
    Universe p l -> return $ Universe p l
    Hole p n -> return $ Hole p n
    Meta p m -> return $ Meta p m
    Builtin p op -> return $ Builtin p op
    App p fun args -> App p <$> cap fun <*> traverse cap args
    Pi p binder result -> Pi p <$> cap binder <*> cap result
    Let p bound binder body -> Let p <$> cap bound <*> cap binder <*> cap body
    Lam p binder body -> Lam p <$> cap binder <*> cap body
    BoundVar p v -> return $ BoundVar p v
    FreeVar p ident -> FreeVar p <$> capitaliseIdentifier ident

instance CapitaliseTypes (Arg var builtin) where
  cap Arg {..} = do
    argExpr' <- cap argExpr
    return $ Arg {argExpr = argExpr', ..}

instance CapitaliseTypes (Binder var builtin) where
  cap Binder {..} = do
    binderType' <- cap binderType
    return $ Binder {binderType = binderType', ..}

capitaliseIdentifier :: (MonadCapitalise m) => Identifier -> m Identifier
capitaliseIdentifier ident@(Identifier m s) = do
  typeIdentifiers <- get
  return $
    Identifier m $
      if member ident typeIdentifiers
        then capitaliseFirstLetter s
        else s

isTypeDef :: (HasStandardTypes builtin) => Expr var builtin -> Bool
isTypeDef t = case t of
  -- We don't capitalise things of type `Bool` because they will be lifted
  -- to the type level, only things of type `X -> Bool`.
  Pi _ _ result -> go result
  _ -> False
  where
    go :: (HasStandardTypes builtin) => Expr var builtin -> Bool
    go (BoolType _) = True
    go (Pi _ _ res) = go res
    go _ = False
