module Vehicle.Compile.CapitaliseTypeNames
  ( capitaliseTypeNames
  ) where

import Control.Monad.State ( MonadState(..), when, modify, evalState )
import Data.Set ( Set, insert, member )
import Data.Functor.Foldable (Recursive(..))

import Vehicle.Compile.Prelude

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
    VarF  p v@(Bound _)       -> return $ Var p v
    VarF  p (Free ident)      -> Var p . Free <$> cap ident

    UniverseF    p l  -> return $ Universe p l
    HoleF        p n  -> return $ Hole p n
    MetaF        p m  -> return $ Meta p m
    LiteralF     p l  -> return $ Literal p l
    BuiltinF     p op -> return $ Builtin p op
    ConstructorF p c  -> return $ Constructor p c

    AnnF  p e t               -> Ann p <$> e <*> t
    AppF  p fun args          -> App p <$> fun <*> traverse sequenceA args
    PiF   p binder result     -> Pi  p <$> sequenceA binder <*> result
    LetF  p bound binder body -> Let p <$> bound <*> sequenceA binder <*> body
    LamF  p binder body       -> Lam p <$> sequenceA binder <*> body
    LVecF p xs                -> LVec p <$> sequence xs

instance CapitaliseTypes Identifier where
  cap ident@(Identifier s) = do
    typeIdentifiers <- get
    return $ Identifier $ if member ident typeIdentifiers
      then capitaliseFirstLetter s
      else s