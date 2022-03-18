{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Backend.LossFunction.Compile where

import Vehicle.Prelude
import Vehicle.Language.AST qualified as V
import Data.Aeson

import GHC.Generics (Generic)
import Data.List.NonEmpty qualified as NonEmpty (map)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe ( fromMaybe, catMaybes )
import Data.ByteString.Lazy (ByteString)
import Vehicle.Compile.Error
import Vehicle.NeuralNetwork
import qualified Vehicle.Language.AST as V
import Vehicle.Compile.Prelude (CheckedDecl)
import qualified Vehicle.Compile.Prelude as V
import Vehicle.Language.Print
import qualified Vehicle.Language.AST.Builtin as V


compile :: MonadCompile m => NetworkMap -> V.CheckedProg -> m [LExpr]
compile _ (V.Main ds) = catMaybes <$> traverse compileDecl ds


compileDecl :: MonadCompile m => V.CheckedDecl -> m (Maybe LExpr)
compileDecl V.DeclNetw {}  = normalisationError currentPass "Network declarations"
compileDecl V.DeclData {}  = normalisationError currentPass "Dataset declarations"
compileDecl (V.DefFun _p _ t expr) = 
    if not $ V.isProperty t
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      then return Nothing
      else Just <$> compileExpr expr


currentPass :: Doc a
currentPass = "compilation to loss functions"

compileArg :: MonadCompile m => V.CheckedArg -> m LExpr
compileArg (V.Arg _ _ e) = compileExpr e

compileLiteral :: V.Literal -> Double
compileLiteral (V.LBool _) = developerError "LBool"
compileLiteral (V.LNat e ) = fromIntegral e
compileLiteral (V.LInt e ) = fromIntegral e
compileLiteral (V.LRat e ) = fromRational e

compileExpr :: MonadCompile m => V.CheckedExpr -> m LExpr
compileExpr e = showExit $ do
  e' <- showEntry e
  case e' of
    V.NotExpr _ _ [e]                     -> Neg <$> compileArg e
    V.AndExpr _ _ [e1, e2]                -> Min <$> compileArg e1 <*> compileArg e2
    V.OrExpr _ _ [e1, e2]                 -> Max <$> compileArg e1 <*> compileArg e2
    V.ImplExpr _ _ [e1, e2]               -> Max <$> (Neg <$> compileArg e1) <*> compileArg e2
     
    V.EqualityExpr V.Eq _ _ _ [e1, e2]    -> Ind <$> compileArg e1 <*> compileArg e2
    V.EqualityExpr V.Neq  _ _ _ [e1, e2]  -> Neg <$> (Ind <$> compileArg e1 <*> compileArg e2)
    V.OrderExpr order _ _ _ [e1, e2]      -> 
      case order of
        V.Le -> Sub <$> compileArg e2 <*> compileArg e1
        V.Lt -> Neg <$> (Sub <$> compileArg e1 <*> compileArg e2)
        V.Ge -> Sub <$> compileArg e1 <*> compileArg e2
        V.Gt -> Neg <$> (Sub <$> compileArg e2 <*> compileArg e1)

    V.LiteralExpr _ _ l                   -> return $ Con (compileLiteral l)
    V.App _ (V.Var _ (V.Free ident)) p    -> NetApp (V.nameOf ident) <$> traverse compileArg p
    V.Var _ (V.Bound t)                   -> return (Var t)
    V.QuantifierExpr q _ binder body      -> Quant q (V.getQuantifierSymbol binder) (Domain ()) <$> compileExpr body
    V.AtExpr _ _ _ [xs, i]                -> At <$> compileArg xs <*> compileArg i
    V.LSeq _ _ xs                         -> TensorLit <$> traverse compileExpr xs
    
    V.Hole{}                              -> developerError "Hole"
    V.Meta{}                              -> developerError "Meta"
    V.Ann{}                               -> developerError "Ann"
    V.Let{}                               -> developerError "Let"
    V.Lam{}                               -> developerError "Lam"
    V.PrimDict{}                          -> developerError "PrimDict"
    V.Pi{}                                -> developerError "Pi"
    V.Type{}                              -> developerError "Type"
    _                                     -> developerError $ unexpectedExprError currentPass (prettySimple e)


showEntry :: MonadCompile m => V.CheckedExpr -> m V.CheckedExpr
showEntry e = do
  logDebug ("loss-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadCompile m => m LExpr -> m LExpr
showExit mNew = do
  new <- mNew
  decrCallDepth
  logDebug ("loss-exit " <+> pretty (show new))
  return new

newtype Domain = Domain () 
  deriving (Eq, Ord, Generic, Show)

data LExpr
  = Neg LExpr
  | Con Double 
  | Min LExpr LExpr
  | Max LExpr LExpr
  | Sub LExpr LExpr
  | Ind LExpr LExpr
  | Var V.DBIndex
  | NetApp Symbol (NonEmpty LExpr)
  | Quant V.Quantifier Symbol Domain LExpr
  | At LExpr LExpr
  | TensorLit [LExpr]
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Domain
instance ToJSON Domain where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON V.Quantifier 
instance ToJSON V.Quantifier where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON LExpr
instance ToJSON LExpr where
  toEncoding = genericToEncoding defaultOptions