module Vehicle.Backend.LossFunction.Compile
  ( LDecl
  , compile
  ) where

import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise (NormalisationOptions (..), normalise)
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Resource (NetworkContext)
import Vehicle.Language.AST.Name (HasName (nameOf))
import Vehicle.Language.Print (prettySimple, prettyVerbose)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Declaration definition

data LDecl
  = DefFunction
    Name                     -- Bound function name.
    LExpr                    -- Bound function body.
  deriving (Eq, Show, Generic)

instance FromJSON LDecl
instance ToJSON LDecl

--------------------------------------------------------------------------------
-- Definitions


data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Quantifier
instance ToJSON Quantifier

newtype Domain = Domain ()
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Domain
instance ToJSON Domain

--definitoon of the LExpr - all expressions allowed in loss constraint

--If changing constructor names, make sure to change it in vehicle.py as well
data LExpr
  = Negation LExpr
  | Constant Double
  | Min LExpr LExpr
  | Max LExpr LExpr
  | Addition LExpr LExpr
  | Subtraction LExpr LExpr
  | Multiplication LExpr LExpr
  | Division LExpr LExpr
  | Negative LExpr
  | IndicatorFunction LExpr LExpr
  | Variable V.DBIndex
  | FreeVariable Name
  | NetworkApplication Name (NonEmpty LExpr)
  | Quantifier Quantifier Name Domain LExpr
  | At LExpr LExpr
  | TensorLiteral [LExpr]
  | Lambda Name LExpr
  deriving (Eq, Ord, Generic, Show)

instance FromJSON LExpr
instance ToJSON LExpr

--------------------------------------------------------------------------------
-- Compilation
-- the translation into the LExpr

compile :: MonadCompile m => V.CheckedProg -> V.PropertyContext -> NetworkContext -> m [LDecl]
compile prog propertyCtx networkCtx = do
  normalisedProg <- normalise prog normalisationOptions
  runReaderT (compileProg normalisedProg) (propertyCtx, networkCtx)

normalisationOptions :: NormalisationOptions
normalisationOptions = Options
  { declContext                 = mempty
  , boundContext                = mempty
  , normaliseDeclApplications   = False
  , normaliseLambdaApplications = False
  , normaliseStdLibApplications = False
  , normaliseBuiltin            = normBuiltin
  , normaliseWeakly             = False
  }

normBuiltin :: V.Builtin -> Bool
normBuiltin b = case b of
  V.TypeClassOp t -> case t of
    V.FromNatTC {} -> True
    V.FromRatTC    -> True
    V.FromVecTC {} -> True
    V.NotTC        -> True
    V.AndTC        -> True
    V.OrTC         -> True
    V.ImpliesTC    -> True
    V.MapTC        -> True
    V.NegTC        -> True
    V.AddTC        -> True
    V.SubTC        -> True
    V.MulTC        -> True
    V.DivTC        -> True

    _              -> False

  V.FromNat {}       -> True
  V.FromRat {}       -> True
  V.FromVec {}       -> True
  V.Foreach{}        -> True

  _                  -> False

compileProg :: MonadCompileLoss m => V.CheckedProg -> m [LDecl]
compileProg  (V.Main ds) = catMaybes <$> traverse compileDecl ds

type MonadCompileLoss m =
  ( MonadCompile m
  , MonadReader (V.PropertyContext, NetworkContext) m
  )

compileDecl :: MonadCompileLoss m => V.CheckedDecl -> m (Maybe LDecl)
compileDecl d =
  case d of
  V.DefResource{} ->
    normalisationError currentPass "resource declarations"

  V.DefPostulate{} ->
    normalisationError currentPass "postulates"

  V.DefFunction _ ident _ expr -> do
    expr' <- compileExpr expr
    logDebug MaxDetail ("loss-declaration " <> prettySimple expr)
    return (Just (DefFunction (nameOf ident) expr'))

currentPass :: Doc a
currentPass = "compilation to loss functions"

compileArg :: MonadCompile m => V.CheckedArg -> m LExpr
compileArg arg = compileExpr (V.argExpr arg)

compileLiteral :: V.Literal -> Double
compileLiteral = \case
  V.LUnit{}    -> developerError "LUnit"
  V.LBool{}    -> developerError "LBool"
  V.LIndex _ e -> fromIntegral e
  V.LNat     e -> fromIntegral e
  V.LInt     e -> fromIntegral e
  V.LRat     e -> fromRational e

compileExpr :: MonadCompile m => V.CheckedExpr -> m LExpr
compileExpr e = showExit $ do
  e' <- showEntry e
  case e' of
    --logical operatives
    V.NotExpr     _ [e1]     -> Negation <$> compileArg e1
    V.AndExpr     _ [e1, e2] -> Min <$> compileArg e1 <*> compileArg e2
    V.OrExpr      _ [e1, e2] -> Max <$> compileArg e1 <*> compileArg e2
    V.ImpliesExpr _ [e1, e2] -> Max <$> (Negation <$> compileArg e1) <*> compileArg e2

    --arithmetic operations
    V.AddExpr   _ _ [e1, e2] -> Addition <$> compileArg e1 <*> compileArg e2
    V.SubExpr   _ _ [e1, e2] -> Subtraction <$> compileArg e1 <*> compileArg e2
    V.MulExpr   _ _ [e1, e2] -> Multiplication <$> compileArg e1 <*> compileArg e2
    V.DivExpr   _ _ [e1, e2] -> Division <$> compileArg e1 <*> compileArg e2
    V.NegExpr   _ _ [e1]     -> Negative <$> compileArg e1

    V.EqualityTCExpr _ op _ _ _ [e1, e2] -> case op of
      V.Eq  -> IndicatorFunction <$> compileArg e1 <*> compileArg e2
      V.Neq -> Negation <$> (IndicatorFunction <$> compileArg e1 <*> compileArg e2)

    V.OrderTCExpr    _ order _ _ _ [e1, e2] ->
      case order of
        V.Le -> Subtraction <$> compileArg e2 <*> compileArg e1
        V.Lt -> Negation <$> (Subtraction <$> compileArg e1 <*> compileArg e2)
        V.Ge -> Subtraction <$> compileArg e1 <*> compileArg e2
        V.Gt -> Negation <$> (Subtraction <$> compileArg e2 <*> compileArg e1)

    V.VecLiteral _ _ xs                -> TensorLiteral <$> traverse compileExpr xs
    V.Literal _ l                      -> return $ Constant $ compileLiteral l
    V.App _ (V.Var _ (V.Free ident)) p -> NetworkApplication (V.nameOf ident) <$> traverse compileArg p
    V.Var _ (V.Bound t)                -> return (Variable t)
    V.AtExpr _ _ _ [xs, i]             -> At <$> compileArg xs <*> compileArg i

    V.QuantifierTCExpr _ q binder body         -> do
      body' <- compileExpr body
      let varName = V.getBinderName binder
      return $ Quantifier (compileQuant q) varName (Domain ()) body'

    V.Hole{}     -> resolutionError "lossFunction" "Hole"
    V.Meta{}     -> resolutionError "lossFunction" "Meta"
    V.Ann{}      -> normalisationError "lossFunction" "Ann"
    V.Let{}      -> normalisationError "lossFunction" "Let"
    V.Lam{}      -> normalisationError "lossFunction" "Lam"
    V.Pi{}       -> unexpectedTypeInExprError "lossFunction" "Pi"
    V.Universe{} -> unexpectedTypeInExprError "lossFunction" "Universe"
    V.IfExpr{}   -> unexpectedExprError "lossFunction" "If statements are not handled at the moment (possibly in the future)"
    _            -> unexpectedExprError currentPass (prettyVerbose e)


compileQuant :: V.Quantifier -> Quantifier
compileQuant V.Forall = All
compileQuant V.Exists = Any

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
