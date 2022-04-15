{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Compile.Delaborate.Internal
  ( Delaborate( delab, delabWithLogging )
  ) where

import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (pack)

import Vehicle.Internal.Abs qualified as B

import Vehicle.Compile.Prelude
import Vehicle.Language.AST qualified as V

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = MonadLogger m

-- * Conversion

class Delaborate t bnfc | t -> bnfc, bnfc -> t where
  delabM :: MonadDelab m => t ann -> m bnfc

  -- | Delaborates the program and throws away the logs, should only be used in
  -- user-facing error messages
  delab :: t ann -> bnfc
  delab = discardLogger . delabM

  delabWithLogging ::  MonadDelab m => t ann -> m bnfc
  delabWithLogging x = do
    logDebug "Beginning delaboration"
    result <- delabM x
    logDebug "Ending delaboration\n"
    return result

-- |Elaborate programs.
instance Delaborate (V.Prog Symbol Symbol) B.Prog where
  delabM (V.Main decls) = B.Main <$> traverse delabM decls

-- |Elaborate declarations.
instance Delaborate (V.Decl Symbol Symbol) B.Decl where
  delabM = \case
    V.DefResource _ Network   n t -> B.DeclNetw  (delabIdentifier n) <$> delabM t
    V.DefResource _ Dataset   n t -> B.DeclData  (delabIdentifier n) <$> delabM t
    V.DefResource _ Parameter n t -> B.DeclParam (delabIdentifier n) <$> delabM t
    V.DefFunction _ n t e         -> B.DefFun    (delabIdentifier n) <$> delabM t <*> delabM e

instance Delaborate (V.Expr Symbol Symbol) B.Expr where
  delabM expr = case expr of
    V.Type _ l     -> return $ B.Type (mkToken B.TypeToken (pack $ "Type" <> show l))
    V.Var _ n      -> return $ B.Var  (delabSymbol n)
    V.Hole _ n     -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l  -> return $ delabLiteral l
    V.Builtin _ op -> return $ B.Builtin (delabBuiltin op)

    V.Ann _ e t    -> B.Ann <$> delabM e <*> delabM t
    V.Pi  _ b t    -> B.Pi  <$> delabM b <*> delabM t
    V.LSeq _ _ es  -> B.LSeq <$> traverse delabM es

    V.Let _ v b e  -> B.Let <$> delabM b <*> delabM v <*> delabM e
    V.Lam _ b e    -> B.Lam <$> delabM b <*> delabM e
    V.Meta _ m     -> return $ B.Hole (mkToken B.HoleToken (layoutAsText (pretty m)))

    V.App _ fun args -> delabApp <$> delabM fun <*> traverse delabM (reverse (NonEmpty.toList args))

    -- This is a hack to get printing of PrimDicts to work without explicitly
    -- including them in the grammar
    V.PrimDict _ e -> B.App (B.Var (delabSymbol "PrimDict")) . B.ExplicitArg <$> delabM e

instance Delaborate (V.Arg Symbol Symbol) B.Arg where
  delabM (V.Arg _i v e) = case v of
    V.Explicit -> B.ExplicitArg <$> delabM e
    V.Implicit -> B.ImplicitArg <$> delabM e
    V.Instance -> B.InstanceArg <$> delabM e

instance Delaborate (V.Binder Symbol Symbol) B.Binder where
  delabM (V.Binder _ann v n t) = case v of
    -- TODO track whether type was provided manually and so use ExplicitBinderAnn
    V.Explicit -> B.ExplicitBinder (delabSymbol n) <$> delabM t
    V.Implicit -> B.ImplicitBinder (delabSymbol n) <$> delabM t
    V.Instance -> B.InstanceBinder (delabSymbol n) <$> delabM t

delabLiteral :: V.Literal -> B.Expr
delabLiteral l = case l of
  V.LBool b -> delabBoolLit b
  V.LNat n  -> delabNatLit n
  V.LInt i  -> if i >= 0
    then delabNatLit i
    else B.App (B.Builtin (delabBuiltin V.Neg)) $ B.ExplicitArg (delabNatLit (-i))
  V.LRat r  -> delabRatLit r

delabBoolLit :: Bool -> B.Expr
delabBoolLit b = B.Literal $ B.LitBool  (mkToken B.BoolToken (if b then "True" else "False"))

delabNatLit :: Int -> B.Expr
delabNatLit n = B.Literal $ B.LitNat (mkToken B.Natural (pack $ show n))

delabRatLit :: Rational -> B.Expr
delabRatLit r = B.Literal $ B.LitRat (mkToken B.Rational (pack $ show (fromRational r :: Double)))

delabSymbol :: Symbol -> B.NameToken
delabSymbol = mkToken B.NameToken

delabIdentifier :: V.Identifier -> B.NameToken
delabIdentifier (V.Identifier n) = mkToken B.NameToken n

delabBuiltin :: V.Builtin -> B.BuiltinToken
delabBuiltin op = mkToken B.BuiltinToken $ V.symbolFromBuiltin op

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun []           = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg