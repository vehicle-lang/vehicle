module Vehicle.Backend.LossFunction.Compile
  ( LDecl
  , DifferentiableLogic
  , compile
  ) where

import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes, fromMaybe)
import GHC.Generics (Generic)

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise (NormalisationOptions (..), normalise)
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Backend.Prelude (DifferentiableLogic (..))
import Vehicle.Compile.Resource (NetworkContext)
import Vehicle.Language.AST.Name (HasName (nameOf))
import Vehicle.Language.Print (prettySimple, prettyVerbose)
import Vehicle.Prelude
import Vehicle.Compile.Queries.DNF
import Vehicle.Language.AST.Arg (argExpr)
import Vehicle.Backend.LossFunction.Logics (LExpr (..), Domain (..), Quantifier (..), DifferentialLogicImplementation (..), dl2Translation, godelTranslation, lukasiewiczTranslation, productTranslation, yagerTranslation)



--------------------------------------------------------------------------------
-- Declaration definition

data LDecl
  = DefFunction
    Name                     -- Bound function name.
    LExpr                    -- Bound function body.
  deriving (Eq, Show, Generic)

instance FromJSON LDecl
instance ToJSON LDecl

instance FromJSON Domain
instance ToJSON Domain

instance FromJSON LExpr
instance ToJSON LExpr

instance FromJSON Quantifier
instance ToJSON Quantifier

--------------------------------------------------------------------------------
-- Compilation
-- the translation into the LExpr (this is the exported top compile function)

compile :: MonadCompile m => DifferentiableLogic -> V.CheckedProg -> V.PropertyContext -> NetworkContext -> m [LDecl]
compile d prog propertyCtx networkCtx = do
  normalisedProg <- normalise prog normalisationOptions
  runReaderT (compileProg (chooseTranslation d) normalisedProg) (propertyCtx, networkCtx)

chooseTranslation :: DifferentiableLogic -> DifferentialLogicImplementation
chooseTranslation = \case
    DL2 -> dl2Translation
    Godel -> godelTranslation
    Lukasiewicz -> lukasiewiczTranslation
    Product -> productTranslation
    Yager -> yagerTranslation


-- |compile entire specification (calls compileDecl)
compileProg :: MonadCompileLoss m => DifferentialLogicImplementation -> V.CheckedProg -> m [LDecl]
compileProg  t (V.Main ds) = catMaybes <$> traverse (compileDecl t) ds

type MonadCompileLoss m =
  ( MonadCompile m
  , MonadReader (V.PropertyContext, NetworkContext) m
  )

-- |compile all functions found in spec, save their names (call compileExpr on each)
compileDecl :: MonadCompileLoss m => DifferentialLogicImplementation -> V.CheckedDecl -> m (Maybe LDecl)
compileDecl t d =
  case d of
  V.DefResource{} ->
    normalisationError currentPass "resource declarations"

  V.DefPostulate{} ->
    normalisationError currentPass "postulates"

  V.DefFunction _ ident _ expr -> do
    expr' <- compileExpr t expr
    logDebug MaxDetail ("loss-declaration " <> prettySimple expr)
    return (Just (DefFunction (nameOf ident) expr'))

currentPass :: Doc a
currentPass = "compilation to loss functions"

compileArg :: MonadCompile m => DifferentialLogicImplementation -> V.CheckedArg -> m LExpr
compileArg t arg = compileExpr t (V.argExpr arg)

-- |helper function for compiling Literals
compileLiteral :: DifferentialLogicImplementation -> V.Literal -> Double
compileLiteral t l = case l of
  V.LUnit{}        -> developerError "Loss Function should not encounter LUnit"
  V.LBool    True  -> compileTrue t
  V.LBool    False -> compileFalse t
  V.LIndex _ e     -> fromIntegral e
  V.LNat     e     -> fromIntegral e
  V.LInt     e     -> fromIntegral e
  V.LRat     e     -> fromRational e

-- |helps compile a name from DBBinding, even if there is no name given
compileDBBinding :: Maybe Name -> Name
compileDBBinding = fromMaybe "No_name"

--compileMaybeNot :: m LExpr -> m LExpr
--compileMaybeNot not = case not of
--  Nothing -> (lowerNot (argExpr e1))

-- |compile a property or single expression
compileExpr :: MonadCompile m => DifferentialLogicImplementation -> V.CheckedExpr -> m LExpr
compileExpr t e = showExit $ do
  e' <- showEntry e
  case e' of
    --logical operatives
    V.NotExpr     _ [e1]     -> case compileNot t of
                                  Nothing -> compileExpr t (lowerNot (argExpr e1))--compileNot t <$> compileExpr t (lowerNot (argExpr e1))
                                  Just f -> f <$> compileArg t e1
    V.AndExpr     _ [e1, e2] -> compileAnd t <$> compileArg t e1 <*> compileArg t e2
    V.OrExpr      _ [e1, e2] -> compileOr t <$> compileArg t e1 <*> compileArg t e2
    V.ImpliesExpr _ [e1, e2] -> compileImplies t <$> (Negation <$> compileArg t e1) <*> compileArg t e2

    --arithmetic operations
    V.AddExpr   _ _ [e1, e2] -> Addition <$> compileArg t e1 <*> compileArg t e2
    V.SubExpr   _ _ [e1, e2] -> Subtraction <$> compileArg t e1 <*> compileArg t e2
    V.MulExpr   _ _ [e1, e2] -> Multiplication <$> compileArg t e1 <*> compileArg t e2
    V.DivExpr   _ _ [e1, e2] -> Division <$> compileArg t e1 <*> compileArg t e2
    V.NegExpr   _ _ [e1]     -> Negation <$> compileArg t e1

    V.EqualityTCExpr _ op _ _ _ [e1, e2] -> case op of
      V.Neq  -> compileNeq t <$> compileArg t e1 <*> compileArg t e2
      V.Eq   -> compileEq t <$> (Max (Constant 0) <$> (Subtraction <$> compileArg t e1 <*> compileArg t e2)) <*> (Max (Constant 0) <$> (Subtraction <$> compileArg t e2 <*> compileArg t e1))

    V.OrderTCExpr    _ order _ _ _ [e1, e2] ->
      case order of
        V.Le -> compileLe t <$> compileArg t e1 <*> compileArg t e2
        V.Lt -> compileLt t <$> compileArg t e1 <*> compileArg t e2
        V.Ge -> compileGe t <$> compileArg t e1 <*> compileArg t e2
        V.Gt -> compileGt t <$> compileArg t e1 <*> compileArg t e2

    V.VecLiteral _ _ xs                -> TensorLiteral <$> traverse (compileExpr t) xs
    V.Literal _ l                      -> return $ Constant $ compileLiteral t l
    V.App _ (V.Var _ (V.Free ident)) p -> NetworkApplication (V.nameOf ident) <$> traverse (compileArg t) p
    V.Var _ (V.Bound var)              -> return (Variable var)
    V.AtExpr _ _ _ [xs, i]             -> At <$> compileArg t xs <*> compileArg t i
    V.Let _ x binder expression          -> Let (compileDBBinding (V.binderRepresentation binder)) <$> compileExpr t x <*> compileExpr t expression
    V.Lam _ binder x                     -> Lambda (compileDBBinding (V.binderRepresentation binder)) <$> compileExpr t x

    V.QuantifierTCExpr _ q binder body         -> do
      body' <- compileExpr t body
      let varName = V.getBinderName binder
      return $ Quantifier (compileQuant q) varName (Domain ()) body'

    V.Hole{}     -> resolutionError "lossFunction" "Should not enounter Hole"
    V.Meta{}     -> resolutionError "lossFunction" "Should not enounter Meta"
    V.Ann{}      -> normalisationError "lossFunction" "Should not enounter Ann"
    V.Pi{}       -> unexpectedTypeInExprError "lossFunction" "Should not enounter Pi"
    V.Universe{} -> unexpectedTypeInExprError "lossFunction" "Should not enounter Universe"
    V.IfExpr{}   -> unexpectedExprError "lossFunction" "If statements are not handled at the moment (possibly in the future)"
    _            -> unexpectedExprError currentPass (prettyVerbose e)


compileQuant :: V.Quantifier -> Quantifier
compileQuant V.Forall = All
compileQuant V.Exists = Any


----------------------------------------------------------------------------------------------------
--handling normalisation options


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



-----------------------------------------------------------------------
-- debugging options

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
