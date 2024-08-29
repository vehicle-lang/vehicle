module Vehicle.Backend.Agda.CapitaliseTypeNames
  ( capitaliseTypeNames,
  )
where

import Control.Monad (when)
import Control.Monad.State (MonadState (..), evalState, modify)
import Data.Set (Set, insert, member)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface

--------------------------------------------------------------------------------
-- Capitalise type names

-- | In Agda types (i.e. functions whose result type is `Set`) are capitalised by
-- convention. This pass identifies all such defined functions and capitalises
-- all references to them. Cannot be done during the main compilation pass as we
-- need to be able to distinguish between free and bound variables.
capitaliseTypeNames :: (BuiltinHasStandardTypes builtin) => Prog builtin -> Prog builtin
capitaliseTypeNames prog = evalState (cap prog) mempty

--------------------------------------------------------------------------------
-- Algorithm

type MonadCapitalise m = MonadState (Set Identifier) m

class CapitaliseTypes a where
  cap :: (MonadCapitalise m) => a -> m a

instance (BuiltinHasStandardTypes builtin) => CapitaliseTypes (Prog builtin) where
  cap (Main ds) = Main <$> traverse cap ds

instance (BuiltinHasStandardTypes builtin) => CapitaliseTypes (Decl builtin) where
  cap d = case d of
    DefAbstract p ident r t ->
      DefAbstract p <$> capitaliseIdentifier ident <*> pure r <*> cap t
    DefFunction p ident anns t e -> do
      when (isTypeDef t) $
        modify (insert ident)
      DefFunction p <$> capitaliseIdentifier ident <*> pure anns <*> cap t <*> cap e

instance CapitaliseTypes (Expr builtin) where
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
    FreeVar p ident -> FreeVar p <$> capitaliseIdentifier ident

instance CapitaliseTypes (Arg builtin) where
  cap Arg {..} = do
    argExpr' <- cap argExpr
    return $ Arg {argExpr = argExpr', ..}

instance CapitaliseTypes (Binder builtin) where
  cap Binder {..} = do
    binderValue' <- cap binderValue
    return $ Binder {binderValue = binderValue', ..}

capitaliseIdentifier :: (MonadCapitalise m) => Identifier -> m Identifier
capitaliseIdentifier ident@(Identifier m s) = do
  typeIdentifiers <- get
  return $
    Identifier m $
      if member ident typeIdentifiers
        then capitaliseFirstLetter s
        else s

isTypeDef :: (BuiltinHasStandardTypes builtin) => Expr builtin -> Bool
isTypeDef t = case t of
  -- We don't capitalise things of type `Bool` because they will be lifted
  -- to the type level, only things of type `X -> Bool`.
  Pi _ _ result -> go result
  _ -> False
  where
    go :: (BuiltinHasStandardTypes builtin) => Expr builtin -> Bool
    go (IBoolType _) = True
    go (Pi _ _ res) = go res
    go _ = False
