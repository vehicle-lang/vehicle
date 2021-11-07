{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Language.Delaborate.Core
  ( Delaborate
  , runDelab
  , runDelabWithoutLogging
  ) where

import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Core.Abs qualified as B

import Vehicle.Prelude
import Vehicle.Language.AST qualified as V

runDelab :: Delaborate a b => a -> Logger b
runDelab x = do
  -- TODO filter out free variables from the expression in the supply monad
  logDebug "Beginning delaboration"
  result <- runSupplyT (delab x) V.freshNames
  logDebug "Ending delaboration\n"
  return result

-- | Delaborates the program and throws away the logs, should only be used in
-- user-facing error messages
runDelabWithoutLogging :: Delaborate a b => a -> b
runDelabWithoutLogging x = discardLogger $ runDelab x

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = (MonadLogger m, MonadSupply Symbol m)

-- * Conversion

class Delaborate vf vc where
  delab :: MonadDelab m => vf -> m vc

-- |Elaborate programs.
instance Delaborate (V.Prog V.Name ann) B.Prog where
  delab (V.Main decls) = B.Main <$> traverse delab decls

-- |Elaborate declarations.
instance Delaborate (V.Decl V.Name ann) B.Decl where
  delab = \case
    V.DeclNetw _ n t -> B.DeclNetw <$> delab n <*> delab t
    V.DeclData _ n t -> B.DeclData <$> delab n <*> delab t
    V.DefFun _ n t e -> B.DefFun   <$> delab n <*> delab t <*> delab e

instance Delaborate (V.Expr V.Name ann) B.Expr where
  delab expr = case expr of
    V.Type l       -> return $ B.Type (mkToken B.TypeToken (pack $ "Type" <> show l))
    V.Var _ n      -> B.Var  <$> delab n
    V.Hole _ n     -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l  -> B.Literal <$> delab l

    V.Ann _ e t    -> B.Ann <$> delab e <*> delab t
    V.Pi  _ b t    -> B.Pi  <$> delab b <*> delab t
    V.Seq _ es     -> B.Seq <$> traverse delab es

    V.Let _ v b e  -> B.Let <$> delab b <*> delab v <*> delab e
    V.Lam _ b e    -> B.Lam <$> delab b <*> delab e
    V.Meta _ m     -> return $ B.Hole (mkToken B.HoleToken (layoutAsText (pretty m)))
    V.PrimDict _   -> developerError "Instance arguments not currently in grammar"

    V.App _ fun args -> delabApp <$> delab fun <*> traverse delab (reverse (NonEmpty.toList args))
    V.Builtin _ op   -> B.Builtin <$> delab op

instance Delaborate (V.Arg V.Name ann) B.Arg where
  delab (V.Arg _i v e) = case v of
    V.Explicit -> B.ExplicitArg <$> delab e
    V.Implicit -> B.ImplicitArg <$> delab e
    V.Instance -> B.InstanceArg <$> delab e

instance Delaborate V.Name B.NameToken where
  delab (V.User s) = return $ mkToken B.NameToken s
  delab V.Machine  = mkToken B.NameToken <$> demand

instance Delaborate V.Identifier B.NameToken where
  delab (V.Identifier n) = return $ mkToken B.NameToken n

instance Delaborate V.Builtin B.BuiltinToken where
  delab op = return $ mkToken B.BuiltinToken $ V.symbolFromBuiltin op

instance Delaborate (V.Binder V.Name ann) B.Binder where
  delab (V.Binder _p _i v n t) = case v of
    -- TODO track whether type was provided manually and so use ExplicitBinderAnn
    V.Explicit -> B.ExplicitBinder <$> delab n <*> delab t
    V.Implicit -> B.ImplicitBinder <$> delab n <*> delab t
    V.Instance -> B.InstanceBinder <$> delab n <*> delab t

instance Delaborate V.Literal B.Lit where
  delab l = return $ case l of
    V.LBool b -> B.LitBool  (mkToken B.BoolToken (if b then "True" else "False"))
    V.LNat n  -> B.LitNat   (fromIntegral n)
    V.LInt i  -> B.LitInt   (fromIntegral i)
    V.LRat r  -> B.LitReal  r

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun []           = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg