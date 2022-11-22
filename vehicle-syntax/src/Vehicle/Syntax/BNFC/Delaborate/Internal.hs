{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Syntax.BNFC.Delaborate.Internal
  ( Delaborate
  , delab
  ) where

import Control.Monad.Identity (Identity (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Text (Text, pack)
import Prettyprinter (Pretty (..))

import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.Internal.Abs qualified as B
import Vehicle.Syntax.Parse.Error
import Vehicle.Syntax.Parse.Token
import Vehicle.Syntax.Prelude (layoutAsText)

--------------------------------------------------------------------------------
-- Conversion to BNFC AST

delab :: (Show t, Delaborate t bnfc) => t -> bnfc
delab e = runIdentity (delabM e)

--------------------------------------------------------------------------------
-- Implementation

-- | Constraint for the monad stack used by the elaborator.
type MonadDelab m = Monad m

-- * Conversion

class Delaborate t bnfc | t -> bnfc, bnfc -> t where
  delabM :: MonadDelab m => t -> m bnfc

-- |Elaborate programs.
instance Delaborate V.NamedProg B.Prog where
  delabM (V.Main decls) = B.Main <$> traverse delabM decls

-- |Elaborate declarations.
instance Delaborate V.NamedDecl B.Decl where
  delabM = \case
    V.DefPostulate _  n t -> B.DeclPost      (delabIdentifier n) <$> delabM t
    V.DefFunction _ n t e -> B.DefFun (delabIdentifier n) <$> delabM t <*> delabM e
    V.DefResource _ r n t -> do
      let constructor = case r of
            V.Network            -> B.DeclNetw
            V.Dataset            -> B.DeclData
            V.Parameter          -> B.DeclParam
            V.InferableParameter -> B.DeclImplParam
      constructor (delabIdentifier n) <$> delabM t

instance Delaborate V.NamedExpr B.Expr where
  delabM expr = case expr of
    V.Universe _ u -> return $ delabUniverse u
    V.Var _ n      -> return $ B.Var  (delabSymbol n)
    V.Hole _ n     -> return $ B.Hole (mkToken B.HoleToken n)
    V.Literal _ l  -> return $ delabLiteral l
    V.Builtin _ op -> return $ delabBuiltin op

    V.Ann _ e t    -> B.Ann <$> delabM e <*> delabM t
    V.Pi  _ b t    -> B.Pi  <$> delabM b <*> delabM t
    V.LVec _ es    -> B.LVec <$> traverse delabM es

    V.Let _ v b e  -> B.Let <$> delabM b <*> delabM v <*> delabM e
    V.Lam _ b e    -> B.Lam <$> delabM b <*> delabM e
    V.Meta _ m     -> return $ B.Hole (mkToken B.HoleToken (layoutAsText (pretty m)))

    V.App _ fun args -> delabApp <$> delabM fun <*> traverse delabM (reverse (NonEmpty.toList args))

instance Delaborate V.NamedArg B.Arg where
  delabM (V.Arg _ v r e) = case (v, r) of
    (V.Explicit, V.Relevant)   -> B.RelevantExplicitArg   <$> delabM e
    (V.Implicit, V.Relevant)   -> B.RelevantImplicitArg   <$> delabM e
    (V.Instance, V.Relevant)   -> B.RelevantInstanceArg   <$> delabM e
    (V.Explicit, V.Irrelevant) -> B.IrrelevantExplicitArg <$> delabM e
    (V.Implicit, V.Irrelevant) -> B.IrrelevantImplicitArg <$> delabM e
    (V.Instance, V.Irrelevant) -> B.IrrelevantInstanceArg <$> delabM e

instance Delaborate V.NamedBinder B.Binder where
  delabM (V.Binder _ v r n t) = case (v, r) of
    (V.Explicit, V.Relevant)   -> B.RelevantExplicitBinder   (delabSymbol n) <$> delabM t
    (V.Implicit, V.Relevant)   -> B.RelevantImplicitBinder   (delabSymbol n) <$> delabM t
    (V.Instance, V.Relevant)   -> B.RelevantInstanceBinder   (delabSymbol n) <$> delabM t
    (V.Explicit, V.Irrelevant) -> B.IrrelevantExplicitBinder (delabSymbol n) <$> delabM t
    (V.Implicit, V.Irrelevant) -> B.IrrelevantImplicitBinder (delabSymbol n) <$> delabM t
    (V.Instance, V.Irrelevant) -> B.IrrelevantInstanceBinder (delabSymbol n) <$> delabM t

delabUniverse :: V.Universe -> B.Expr
delabUniverse = \case
  V.TypeUniv l    -> B.Type (mkToken B.TypeToken ("Type" <> pack (show l)))
  V.PolarityUniv  -> B.Builtin $ mkBuiltinToken V.PolarityUniv
  V.LinearityUniv -> B.Builtin $ mkBuiltinToken V.LinearityUniv
  where
    mkBuiltinToken :: Pretty a => a -> B.BuiltinToken
    mkBuiltinToken a = mkToken B.BuiltinToken $ layoutAsText (pretty a)

delabLiteral :: V.Literal -> B.Expr
delabLiteral l = case l of
  V.LUnit      -> B.Literal B.UnitLiteral
  V.LBool b    -> delabBoolLit b
  V.LIndex _ n -> delabNatLit n
  V.LNat n     -> delabNatLit n
  V.LInt i     -> if i >= 0
    then delabNatLit i
    else B.App (delabBuiltin (V.Neg V.NegInt)) $ B.RelevantExplicitArg (delabNatLit (-i))
  V.LRat r     -> delabRatLit r

delabBoolLit :: Bool -> B.Expr
delabBoolLit b = B.Literal $ B.BoolLiteral  (mkToken B.BoolToken (if b then "True" else "False"))

delabNatLit :: Int -> B.Expr
delabNatLit n = B.Literal $ B.NatLiteral (mkToken B.Natural (pack $ show n))

delabRatLit :: Rational -> B.Expr
delabRatLit r = B.Literal $ B.RatLiteral (mkToken B.Rational (pack $ show (fromRational r :: Double)))

delabSymbol :: Text -> B.NameToken
delabSymbol = mkToken B.NameToken

delabIdentifier :: V.Identifier -> B.NameToken
delabIdentifier (V.Identifier n) = mkToken B.NameToken n

delabBuiltin :: V.Builtin -> B.Expr
delabBuiltin op = B.Builtin $ mkToken B.BuiltinToken $ V.symbolFromBuiltin op

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun []           = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg