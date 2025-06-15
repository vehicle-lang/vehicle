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
import Data.Text (Text)
import Vehicle.Syntax.AST qualified as V
import Vehicle.Syntax.Builtin qualified as V
import Vehicle.Syntax.Internal.Abs qualified as B
import Vehicle.Syntax.Parse.Token

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
instance Delaborate V.Prog B.Prog where
  delabM (V.Main decls) = B.Main <$> traverse delabM decls

-- | Elaborate declarations.
instance Delaborate V.Decl B.Decl where
  delabM = \case
    V.DefFunction _ n _ t e -> B.DefFun (delabIdentifier n) <$> delabM t <*> delabM e
    V.DefAbstract _ n s t -> do
      constructor <- delabM s
      constructor (delabIdentifier n) <$> delabM t

instance Delaborate V.DefAbstractSort (B.NameToken -> B.Expr -> B.Decl) where
  delabM sort = return $ case sort of
    V.PostulateDef {} -> B.DeclPost
    V.NetworkDef -> B.DeclNetw
    V.DatasetDef -> B.DeclData
    V.ParameterDef paramSort -> case paramSort of
      V.NonInferable -> B.DeclParam
      V.Inferable -> B.DeclImplParam

instance Delaborate V.Expr B.Expr where
  delabM expr = case expr of
    V.Universe _ -> return delabUniverse
    V.Var _ n -> return $ B.Var (delabSymbol n)
    V.Hole _ n -> return $ B.Hole (mkToken B.HoleToken n)
    V.Builtin _ op -> return $ delabBuiltin op
    V.Pi _ b t -> B.Pi <$> delabM b <*> delabM t
    V.Let _ v b e -> B.Let <$> delabM b <*> delabM v <*> delabM e
    V.Lam _ b e -> B.Lam <$> delabM b <*> delabM e
    V.App fun args -> delabApp <$> delabM fun <*> traverse delabM (reverse (NonEmpty.toList args))

delabRelevance :: (V.HasRelevance a) => a -> [B.Modality]
delabRelevance x = case V.relevanceOf x of
  V.Relevant -> []
  V.Irrelevant -> [B.Irrelevant]

instance Delaborate V.Arg B.Arg where
  delabM :: (MonadDelab m) => V.Arg -> m B.Arg
  delabM arg = do
    expr <- delabM $ V.argExpr arg
    let modalities = delabRelevance arg
    case V.visibilityOf arg of
      V.Explicit {} -> return $ B.ExplicitArg modalities expr
      V.Implicit {} -> return $ B.ImplicitArg modalities expr
      V.Instance {} -> return $ B.InstanceArg modalities expr

instance Delaborate V.Binder B.Binder where
  delabM binder = do
    typ <- delabM $ V.binderValue binder
    let modalities = delabRelevance binder
    let name = delabSymbol $ fromMaybe "_" (V.nameOf binder)
    case V.visibilityOf binder of
      V.Explicit {} -> return $ B.ExplicitBinder modalities name typ
      V.Implicit {} -> return $ B.ImplicitBinder modalities name typ
      V.Instance {} -> return $ B.InstanceBinder modalities name typ

delabUniverse :: B.Expr
delabUniverse = B.Type (mkToken B.TypeToken "Type")

delabSymbol :: Text -> B.NameToken
delabSymbol = mkToken B.NameToken

delabIdentifier :: V.Identifier -> B.NameToken
delabIdentifier (V.Identifier _ n) = mkToken B.NameToken n

delabBuiltin :: V.Builtin -> B.Expr
delabBuiltin op = B.Builtin $ mkToken B.BuiltinToken $ V.symbolFromBuiltin op

delabApp :: B.Expr -> [B.Arg] -> B.Expr
delabApp fun [] = fun
delabApp fun (arg : args) = B.App (delabApp fun args) arg
