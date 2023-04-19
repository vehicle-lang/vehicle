module Vehicle.Compile.CapitaliseTypeNames
  ( capitaliseTypeNames,
  )
where

import Control.Monad (when)
import Control.Monad.State (MonadState (..), evalState, modify)
import Data.Set (Set, insert, member)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns

--------------------------------------------------------------------------------
-- Capitalise type names

-- | In Agda types (i.e. functions whose result type is `Set`) are capitalised by
-- convention. This pass identifies all such defined functions and capitalises
-- all references to them. Cannot be done during the main compilation pass as we
-- need to be able to distinguish between free and bound variables.
capitaliseTypeNames :: TypeCheckedProg -> TypeCheckedProg
capitaliseTypeNames prog = evalState (cap prog) mempty

isTypeDef :: TypeCheckedExpr -> Bool
isTypeDef t = case t of
  -- We don't capitalise things of type `Bool` because they will be lifted
  -- to the type level, only things of type `X -> Bool`.
  Pi _ _ result -> go result
  _ -> False
  where
    go :: TypeCheckedExpr -> Bool
    go (BoolType _) = True
    go (Pi _ _ res) = go res
    go _ = False

class CapitaliseTypes a where
  cap :: (MonadState (Set Identifier) m) => a -> m a

instance CapitaliseTypes TypeCheckedProg where
  cap (Main ds) = Main <$> traverse cap ds

instance CapitaliseTypes TypeCheckedDecl where
  cap d = case d of
    DefAbstract p ident r t ->
      DefAbstract p <$> cap ident <*> pure r <*> cap t
    DefFunction p ident anns t e -> do
      when (isTypeDef t) $
        modify (insert ident)
      DefFunction p <$> cap ident <*> pure anns <*> cap t <*> cap e

instance CapitaliseTypes TypeCheckedExpr where
  cap = \case
    Universe p l -> return $ Universe p l
    Hole p n -> return $ Hole p n
    Meta p m -> return $ Meta p m
    Builtin p op -> return $ Builtin p op
    Ann p e t -> Ann p <$> cap e <*> cap t
    App p fun args -> App p <$> cap fun <*> traverse cap args -- traverse cap args
    Pi p binder result -> Pi p <$> cap binder <*> cap result
    Let p bound binder body -> Let p <$> cap bound <*> cap binder <*> cap body
    Lam p binder body -> Lam p <$> cap binder <*> cap body
    BoundVar p v -> return $ BoundVar p v
    FreeVar p ident -> FreeVar p <$> cap ident

instance CapitaliseTypes TypeCheckedArg where
  cap Arg {..} = do
    argExpr' <- cap argExpr
    return $ Arg {argExpr = argExpr', ..}

instance CapitaliseTypes TypeCheckedBinder where
  cap Binder {..} = do
    binderType' <- cap binderType
    return $ Binder {binderType = binderType', ..}

instance CapitaliseTypes Identifier where
  cap ident@(Identifier m s) = do
    typeIdentifiers <- get
    return $
      Identifier m $
        if member ident typeIdentifiers
          then capitaliseFirstLetter s
          else s
