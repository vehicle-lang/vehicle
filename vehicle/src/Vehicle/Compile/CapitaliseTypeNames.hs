module Vehicle.Compile.CapitaliseTypeNames
  ( capitaliseTypeNames
  ) where

import Control.Monad.State (MonadState (..), evalState, modify, when)
import Data.Functor.Foldable (Recursive (..))
import Data.Set (Set, insert, member)

import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn

--------------------------------------------------------------------------------
-- Capitalise type names

-- In Agda types (i.e. functions whose result type is `Set`) are capitalised by
-- convention. This pass identifies all such defined functions and capitalises
-- all references to them. Cannot be done during the main compilation pass as we
-- need to be able to distinguish between free and bound variables.

capitaliseTypeNames :: CheckedProg -> CheckedProg
capitaliseTypeNames prog = evalState (cap prog) mempty

isTypeDef :: CheckedExpr -> Bool
isTypeDef t = case t of
  -- We don't capitalise things of type `Bool` because they will be lifted
  -- to the type level, only things of type `X -> Bool`.
  Pi _ _ result -> go result
  _             -> False
  where
    go :: CheckedExpr -> Bool
    go (BoolType _) = True
    go (Pi _ _ res) = go res
    go _            = False

class CapitaliseTypes a where
  cap :: MonadState (Set Identifier) m => a -> m a

instance CapitaliseTypes CheckedProg where
  cap (Main ds) = Main <$> traverse cap ds

instance CapitaliseTypes CheckedDecl where
  cap d = case d of
    DefResource p r ident t ->
      DefResource p r <$> cap ident <*> cap t

    DefFunction p ident t e -> do
      when (isTypeDef t) $
        modify (insert ident)
      DefFunction p <$> cap ident <*> cap t <*> cap e

    DefPostulate p ident t ->
      DefPostulate p <$> cap ident <*> cap t

instance CapitaliseTypes CheckedExpr where
  cap = cata $ \case
    UniverseF ann l                 -> return $ Universe ann l
    HoleF     ann n                 -> return $ Hole ann n
    MetaF     ann m                 -> return $ Meta ann m
    LiteralF  ann l                 -> return $ Literal ann l
    BuiltinF  ann op                -> return $ Builtin ann op
    AnnF      ann e t               -> Ann ann <$> e <*> t
    AppF      ann fun args          -> App ann <$> fun <*> traverse sequenceA args --traverse cap args
    PiF       ann binder result     -> Pi  ann <$> sequenceA binder <*> result
    LetF      ann bound binder body -> Let ann <$> bound <*> sequenceA binder <*> body
    LamF      ann binder body       -> Lam ann <$> sequenceA binder <*> body
    LVecF     ann xs                -> LVec ann <$> sequence xs
    VarF      ann v@(Bound _)       -> return $ Var ann v
    VarF      ann (Free ident)      -> Var ann . Free <$> cap ident

instance CapitaliseTypes Identifier where
  cap ident@(Identifier s) = do
    typeIdentifiers <- get
    return $ Identifier $ if member ident typeIdentifiers
      then capitaliseFirstLetter s
      else s
