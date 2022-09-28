module Vehicle.Backend.LossFunction.Compile
  ( LExpr
  , compile
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT, asks)
import Data.Aeson
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Map qualified as Map (member)

import Vehicle.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource (NetworkContext)
import Vehicle.Language.AST qualified as V
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Public interface

compile :: MonadCompile m => V.CheckedProg -> V.PropertyContext -> NetworkContext -> m [LExpr]
compile prog propertyCtx _networkCtx = do
  runReaderT (compileProg prog) propertyCtx

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
  = Neg LExpr
  | Con Double
  | Min LExpr LExpr
  | Max LExpr LExpr
  | Sub LExpr LExpr
  | Ind LExpr LExpr
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

type MonadCompileLoss m =
  ( MonadCompile m
  , MonadReader V.PropertyContext m
  )

compileProg :: MonadCompileLoss m => V.CheckedProg -> m [LExpr]
compileProg  (V.Main ds) = catMaybes <$> traverse compileDecl ds

compileDecl :: MonadCompileLoss m => V.CheckedDecl -> m (Maybe LExpr)
compileDecl d = case d of
  V.DefResource{} ->
    normalisationError currentPass "resource declarations"

  V.DefPostulate{} ->
    normalisationError currentPass "postulates"

  V.DefFunction _ ident _ expr -> do
    isProperty <- asks (Map.member ident)
    if not isProperty
      -- If it's not a property then we can discard it as all applications
      -- of it should have been normalised out by now.
      then return Nothing
      else Just <$> compileExpr expr

currentPass :: Doc a
currentPass = "compilation to loss functions"

compileArg :: MonadCompile m => V.CheckedArg -> m LExpr
compileArg arg = compileExpr (V.argExpr arg)

compileLiteral :: V.Literal -> Double
compileLiteral = \case
  V.LUnit{} -> developerError "LUnit"
  V.LBool{} -> developerError "LBool"
  V.LIndex _ e -> fromIntegral e
  V.LNat     e -> fromIntegral e
  V.LInt     e -> fromIntegral e
  V.LRat     e -> fromRational e

compileExpr :: MonadCompile m => V.CheckedExpr -> m LExpr
compileExpr e = showExit $ do
  e' <- showEntry e
  case e' of
    V.NotExpr     _ [e1]     -> Neg <$> compileArg e1
    V.AndExpr     _ [e1, e2] -> Min <$> compileArg e1 <*> compileArg e2
    V.OrExpr      _ [e1, e2] -> Max <$> compileArg e1 <*> compileArg e2
    V.ImpliesExpr _ [e1, e2] -> Max <$> (Neg <$> compileArg e1) <*> compileArg e2

    V.EqualityTCExpr _ op _ _ _ [e1, e2] -> case op of
      V.Eq  -> Ind <$> compileArg e1 <*> compileArg e2
      V.Neq -> Neg <$> (Ind <$> compileArg e1 <*> compileArg e2)

    V.OrderExpr    _ _ order [e1, e2] ->
      case order of
        V.Le -> Sub <$> compileArg e2 <*> compileArg e1
        V.Lt -> Neg <$> (Sub <$> compileArg e1 <*> compileArg e2)
        V.Ge -> Sub <$> compileArg e1 <*> compileArg e2
        V.Gt -> Neg <$> (Sub <$> compileArg e2 <*> compileArg e1)

    V.VecLiteral _ _ xs                -> TensorLit <$> traverse compileExpr xs
    V.Literal _ l                      -> return $ Con $ compileLiteral l
    V.App _ (V.Var _ (V.Free ident)) p -> NetApp (V.nameOf ident) <$> traverse compileArg p
    V.Var _ (V.Bound t)                -> return (Var t)
    V.AtExpr _ _ _ [xs, i]             -> At <$> compileArg xs <*> compileArg i

    V.QuantifierTCExpr _ q binder body         -> do
      body' <- compileExpr body
      let varName = V.getBinderSymbol binder
      return $ Quant (compileQuant q) varName (Domain ()) body'

    V.Hole{}     -> resolutionError "lossFunction" "Hole"
    V.Meta{}     -> resolutionError "lossFunction" "Meta"
    V.Ann{}      -> normalisationError "lossFunction" "Ann"
    V.Let{}      -> normalisationError "lossFunction" "Let"
    V.Lam{}      -> normalisationError "lossFunction" "Lam"
    V.Pi{}       -> unexpectedTypeInExprError "lossFunction" "Pi"
    V.Universe{} -> unexpectedTypeInExprError "lossFunction" "Universe"
    _            -> unexpectedExprError currentPass (prettySimple e)


compileQuant :: V.Quantifier -> Quantifier
compileQuant V.Forall  = All
compileQuant V.Exists  = Any

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
