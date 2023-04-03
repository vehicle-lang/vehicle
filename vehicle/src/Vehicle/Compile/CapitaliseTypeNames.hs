module Vehicle.Compile.CapitaliseTypeNames
  ( capitaliseTypeNames,
  )
where

import Control.Monad.State (MonadState (..), evalState, modify)
import Data.Functor.Foldable (Recursive (..))
import Data.Set (Set, insert, member)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Control.Monad (when)

--------------------------------------------------------------------------------
-- Capitalise type names

-- In Agda types (i.e. functions whose result type is `Set`) are capitalised by
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
    DefResource p ident r t ->
      DefResource p <$> cap ident <*> pure r <*> cap t
    DefFunction p ident isProperty t e -> do
      when (isTypeDef t) $
        modify (insert ident)
      DefFunction p <$> cap ident <*> pure isProperty <*> cap t <*> cap e
    DefPostulate p ident t ->
      DefPostulate p <$> cap ident <*> cap t

instance CapitaliseTypes TypeCheckedExpr where
  cap = cata $ \case
    UniverseF p l -> return $ Universe p l
    HoleF p n -> return $ Hole p n
    MetaF p m -> return $ Meta p m
    BuiltinF p op -> return $ Builtin p op
    AnnF p e t -> Ann p <$> e <*> t
    AppF p fun args -> App p <$> fun <*> traverse sequenceA args -- traverse cap args
    PiF p binder result -> Pi p <$> sequenceA binder <*> result
    LetF p bound binder body -> Let p <$> bound <*> sequenceA binder <*> body
    LamF p binder body -> Lam p <$> sequenceA binder <*> body
    BoundVarF p v -> return $ BoundVar p v
    FreeVarF p ident -> FreeVar p <$> cap ident

instance CapitaliseTypes Identifier where
  cap ident@(Identifier m s) = do
    typeIdentifiers <- get
    return $
      Identifier m $
        if member ident typeIdentifiers
          then capitaliseFirstLetter s
          else s
