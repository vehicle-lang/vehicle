{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Vehicle.Syntax.BNFC.Delaborate.Internal
  ( Delaborate,
    delab,
  )
where

import Control.Monad.Identity (Identity (..))
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (fromMaybe)
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
  delabM :: (MonadDelab m) => t -> m bnfc

-- | Elaborate programs.
instance Delaborate (V.Prog V.Name V.Builtin) B.Prog where
  delabM (V.Main decls) = B.Main <$> traverse delabM decls

-- | Elaborate declarations.
instance Delaborate (V.Decl V.Name V.Builtin) B.Decl where
  delabM = \case
    V.DefFunction _ n _ t e -> B.DefFun (delabIdentifier n) <$> delabM t <*> delabM e
    V.DefAbstract _ n s t -> do
      constructor <- delabM s
      constructor (delabIdentifier n) <$> delabM t

instance Delaborate V.DefAbstractSort (B.NameToken -> B.Expr -> B.Decl) where
  delabM sort = return $ case sort of
    V.PostulateDef -> B.DeclPost
    V.NetworkDef -> B.DeclNetw
    V.DatasetDef -> B.DeclData
    V.ParameterDef sort -> case sort of
      V.NonInferable -> B.DeclParam
      V.Inferable -> B.DeclImplParam

instance Delaborate (V.Expr V.Name V.Builtin) B.Expr where
  delabM expr = case expr of
    V.Universe _ u -> return $ delabUniverse u
    V.FreeVar _ n -> return $ B.Var (delabSymbol (V.nameOf n))
    V.BoundVar _ n -> return $ B.Var (delabSymbol n)
    V.Hole _ n -> return $ B.Hole (mkToken B.HoleToken n)
    V.Builtin _ op -> return $ delabBuiltin op
    V.Ann _ e t -> B.Ann <$> delabM e <*> delabM t
    V.Pi _ b t -> B.Pi <$> delabM b <*> delabM t
    V.Let _ v b e -> B.Let <$> delabM b <*> delabM v <*> delabM e
    V.Lam _ b e -> B.Lam <$> delabM b <*> delabM e
    V.Meta _ m -> return $ B.Hole (mkToken B.HoleToken (layoutAsText (pretty m)))
    V.App _ fun args -> delabApp <$> delabM fun <*> traverse delabM (reverse (NonEmpty.toList args))

instance Delaborate (V.Arg V.Name V.Builtin) B.Arg where
  delabM :: (MonadDelab m) => V.Arg V.Name V.Builtin -> m B.Arg
  delabM (V.Arg _ v r e) = case (v, r) of
    (V.Explicit {}, V.Relevant) -> B.RelevantExplicitArg <$> delabM e
    (V.Implicit {}, V.Relevant) -> B.RelevantImplicitArg <$> delabM e
    (V.Instance {}, V.Relevant) -> B.RelevantInstanceArg <$> delabM e
    (V.Explicit {}, V.Irrelevant) -> B.IrrelevantExplicitArg <$> delabM e
    (V.Implicit {}, V.Irrelevant) -> B.IrrelevantImplicitArg <$> delabM e
    (V.Instance {}, V.Irrelevant) -> B.IrrelevantInstanceArg <$> delabM e

instance Delaborate (V.Binder V.Name V.Builtin) B.Binder where
  delabM binder = do
    t' <- delabM $ V.binderType binder
    let n' = delabSymbol $ fromMaybe "_" (V.nameOf binder)
    return $ case (V.visibilityOf binder, V.relevanceOf binder) of
      (V.Explicit {}, V.Relevant) -> B.RelevantExplicitBinder n' t'
      (V.Implicit {}, V.Relevant) -> B.RelevantImplicitBinder n' t'
      (V.Instance {}, V.Relevant) -> B.RelevantInstanceBinder n' t'
      (V.Explicit {}, V.Irrelevant) -> B.IrrelevantExplicitBinder n' t'
      (V.Implicit {}, V.Irrelevant) -> B.IrrelevantImplicitBinder n' t'
      (V.Instance {}, V.Irrelevant) -> B.IrrelevantInstanceBinder n' t'

delabUniverse :: V.UniverseLevel -> B.Expr
delabUniverse = \case
  V.UniverseLevel l -> B.Type (mkToken B.TypeToken ("Type" <> pack (show l)))
  where
    mkBuiltinToken :: (Pretty a) => a -> B.BuiltinToken
    mkBuiltinToken a = mkToken B.BuiltinToken $ layoutAsText (pretty a)

delabBoolLit :: Bool -> B.Expr
delabBoolLit b = B.Literal $ B.BoolLiteral (mkToken B.BoolToken (if b then "True" else "False"))

delabNatLit :: Int -> B.Expr
delabNatLit n = B.Literal $ B.NatLiteral (mkToken B.Natural (pack $ show n))

delabRatLit :: Rational -> B.Expr
delabRatLit r = B.Literal $ B.RatLiteral (mkToken B.Rational (pack $ show (fromRational r :: Double)))

delabSymbol :: Text -> B.NameToken
delabSymbol = mkToken B.NameToken

delabIdentifier :: V.Identifier -> B.NameToken
delabIdentifier (V.Identifier _ n) = mkToken B.NameToken n

delabBuiltin :: V.Builtin -> B.Expr
delabBuiltin op = B.Builtin $ mkToken B.BuiltinToken $ V.symbolFromBuiltin op

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun [] = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg
