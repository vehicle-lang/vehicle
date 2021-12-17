

module Vehicle.Compile.Backend.Agda.Preprocess
  ( preprocess
  ) where

import Control.Monad.State ( MonadState(..), when, modify, evalState )
import Data.Set ( Set, insert, member )
import Data.Functor.Foldable (Recursive(..))

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile.Descope (runDescopeProg)
import Vehicle.Compile.SupplyNames (supplyDBNames)

--------------------------------------------------------------------------------
-- Agda compilation proprocessing step

preprocess :: CheckedProg -> OutputProg
preprocess prog1 = prog4
  where
    prog2 = capitaliseTypeNames prog1
    prog3 = supplyDBNames prog2
    prog4 = runDescopeProg prog3

--------------------------------------------------------------------------------
-- Capitalise type names

-- In Agda types (i.e. functions whose result type is `Set`) are capitalised by
-- convention. This pass identifies all such defined functions and capitalises
-- all references to them. Cannot be done during the main compilation pass as we
-- need to be able to distinguish between free and bound variables.

capitaliseTypeNames :: CheckedProg -> CheckedProg
capitaliseTypeNames prog = evalState (cap prog) mempty

isType :: CheckedExpr -> Bool
isType t = case t of
  -- We don't capitalise things of type `Prop` because they will be lifted
  -- to the type level, only things of type `X -> Prop`.
  Pi _ _ result -> go result
  _             -> False
  where
    go :: CheckedExpr -> Bool
    go (BuiltinBooleanType _ Prop) = True
    go (Pi _ _ res)                = go res
    go _                           = False

class CapitaliseTypes a where
  cap :: MonadState (Set Identifier) m => a -> m a

instance CapitaliseTypes CheckedProg where
  cap (Main ds) = Main <$> traverse cap ds

instance CapitaliseTypes CheckedDecl where
  cap d = case d of
    DeclData p ident t -> DeclData p <$> cap ident <*> cap t
    DeclNetw p ident t -> DeclNetw p <$> cap ident <*> cap t
    DefFun p ident t e -> do
      when (isType t) $
        modify (insert ident)
      DefFun p <$> cap ident <*> cap t <*> cap e

instance CapitaliseTypes CheckedExpr where
  cap = cata $ \case
    TypeF     l                     -> return $ Type l
    HoleF     p n                   -> return $ Hole p n
    MetaF     ann m                 -> return $ Meta ann m
    LiteralF  ann l                 -> return $ Literal ann l
    BuiltinF  ann op                -> return $ Builtin ann op
    PrimDictF ann t                 -> PrimDict ann <$> t
    AnnF      ann e t               -> Ann ann <$> e <*> t
    AppF      ann fun args          -> App ann <$> fun <*> traverse cap args
    PiF       ann binder result     -> Pi  ann <$> cap binder <*> result
    LetF      ann bound binder body -> Let ann <$> bound <*> cap binder <*> body
    LamF      ann binder body       -> Lam ann <$> cap binder <*> body
    LSeqF     ann dict xs           -> LSeq ann <$> dict <*> sequence xs
    VarF      ann v@(Bound _)       -> return $ Var ann v
    VarF      ann (Free ident)      -> Var ann . Free <$> cap ident

instance CapitaliseTypes CheckedArg where
  cap = traverseArgExpr cap

instance CapitaliseTypes CheckedBinder where
  cap = traverseBinderType cap

instance CapitaliseTypes Identifier where
  cap ident@(Identifier s) = do
    typeIdentifiers <- get
    return $ Identifier $ if member ident typeIdentifiers
      then capitaliseFirstLetter s
      else s