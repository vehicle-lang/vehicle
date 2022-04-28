module Vehicle.Backend.LossFunction.Compile
  ( LExpr
  , compile
  ) where

import Data.Aeson
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Resource.NeuralNetwork
import Vehicle.Language.AST qualified as V
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Definitions

data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Quantifier
instance ToJSON Quantifier where
  toEncoding = genericToEncoding defaultOptions

newtype Domain = Domain ()
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Domain
instance ToJSON Domain where
  toEncoding = genericToEncoding defaultOptions

data LExpr
  = Negation LExpr
  | Constant Double
  | Min LExpr LExpr
  | Max LExpr LExpr
  | Substraction LExpr LExpr
  | IndicatorFunction LExpr LExpr
  | Var V.DBIndex
  | NetApp Symbol (NonEmpty LExpr)
  | Quant Quantifier Symbol Domain LExpr
  | At LExpr LExpr
  | TensorLit [LExpr]
  deriving (Eq, Ord, Generic, Show)

instance FromJSON LExpr
instance ToJSON LExpr where
  toEncoding = genericToEncoding defaultOptions

--------------------------------------------------------------------------------
-- Compilation

compile :: MonadCompile m => NetworkCtx -> V.CheckedProg -> m [LExpr]
compile _ (V.Main ds) = catMaybes <$> traverse compileDecl ds

compileDecl :: MonadCompile m => V.CheckedDecl -> m (Maybe LExpr)
compileDecl V.DefResource{} = normalisationError currentPass "Resource declarations"
compileDecl (V.DefFunction _p _ t expr) =
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
    V.NotExpr  _ _ [e1]     -> Negation <$> compileArg e1
    V.AndExpr  _ _ [e1, e2] -> Min <$> compileArg e1 <*> compileArg e2
    V.OrExpr   _ _ [e1, e2] -> Max <$> compileArg e1 <*> compileArg e2
    V.ImplExpr _ _ [e1, e2] -> Max <$> (Negation <$> compileArg e1) <*> compileArg e2

    V.EqualityExpr V.Eq  _ _ _ [e1, e2] -> IndicatorFunction <$> compileArg e1 <*> compileArg e2
    V.EqualityExpr V.Neq _ _ _ [e1, e2] -> Negation <$> (IndicatorFunction <$> compileArg e1 <*> compileArg e2)
    V.OrderExpr    order _ _ _ [e1, e2] ->
      case order of
        V.Le -> Substraction <$> compileArg e2 <*> compileArg e1
        V.Lt -> Negation <$> (Substraction <$> compileArg e1 <*> compileArg e2)
        V.Ge -> Substraction <$> compileArg e1 <*> compileArg e2
        V.Gt -> Negation <$> (Substraction <$> compileArg e2 <*> compileArg e1)

    V.LiteralExpr _ _ l                   -> return $ Constant (compileLiteral l)
    V.App _ (V.Var _ (V.Free ident)) p    -> NetApp (V.nameOf ident) <$> traverse compileArg p
    V.Var _ (V.Bound t)                   -> return (Var t)
    V.QuantifierExpr q _ binder body      -> Quant (compileQuant q) (V.getBinderSymbol binder) (Domain ()) <$> compileExpr body
    V.AtExpr _ _ _ _ [xs, i]              -> At <$> compileArg xs <*> compileArg i
    V.LSeq _ _ xs                         -> TensorLit <$> traverse compileExpr xs

    V.Hole{}                              -> resolutionError "lossFunction" "Hole"
    V.Meta{}                              -> resolutionError "lossFunction" "Meta"  
    V.Let{}                               -> normalisationError "lossFunction" "Let"
    V.Lam{}                               -> normalisationError "lossFunction" "Lam"
    V.PrimDict{}                          -> typeError "lossFunction" "PrimDict"
    V.Pi{}                                -> typeError "lossFunction" "Pi"
    V.Type{}                              -> typeError "lossFunction" "Type"
    _                                     -> compilerDeveloperError $ unexpectedExprError currentPass (prettySimple e)
    V.Ann{}                               -> normalisationError "lossFunction" "Ann"

compileQuant :: V.Quantifier -> Quantifier
compileQuant V.All = All
compileQuant V.Any = Any

showEntry :: MonadCompile m => V.CheckedExpr -> m V.CheckedExpr
showEntry e = do
  logDebug MaxDetail ("loss-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadCompile m => m LExpr -> m LExpr
showExit mNew = do
  new <- mNew
  decrCallDepth
  logDebug MaxDetail ("loss-exit " <+> pretty (show new))
  return new
