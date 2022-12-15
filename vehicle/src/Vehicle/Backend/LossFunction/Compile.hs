module Vehicle.Backend.LossFunction.Compile
  ( LDecl
  , DifferentiableLogic
  , compile
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import Vehicle.Backend.LossFunction.Logics (DifferentialLogicImplementation (..),
                                            Domain (..), LExpr (..),
                                            Quantifier (..), implementationOf)
import Vehicle.Backend.Prelude (DifferentiableLogic (..))
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.Normalise (NormalisationOptions (..), normalise)
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Print (prettySimple, prettyVerbose)
import Vehicle.Compile.Type (getUnnormalised)
import Vehicle.Expr.DeBruijn qualified as V
import Vehicle.Prelude
import Vehicle.Resource (Resources (..))
import Vehicle.Syntax.AST (HasName (nameOf), Name, argExpr)

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
-- Compilation

currentPass :: Doc a
currentPass = "compilation to loss functions"

-- | The translation into the LExpr (this is the exported top compile function)
compile :: (MonadIO m, MonadCompile m)
        => Resources
        -> DifferentiableLogic
        -> V.TypedProg
        -> m [LDecl]
compile resources logic typedProg = do
  (_, expandedProg) <- expandResources resources typedProg
  unnormalisedProg <- traverse getUnnormalised expandedProg
  normalisedProg <- normalise unnormalisedProg normalisationOptions
  compileProg logic normalisedProg

-- |Compile entire specification (calls compileDecl)
compileProg :: MonadCompile m => DifferentiableLogic -> V.CheckedProg -> m [LDecl]
compileProg logic (V.Main ds) = logCompilerPass MinDetail "compilation to loss function" $
  traverse (compileDecl logic) ds

-- |Compile all functions found in spec, save their names (call compileExpr on each)
compileDecl :: MonadCompile m => DifferentiableLogic -> V.CheckedDecl -> m LDecl
compileDecl logic d =
  case d of
  V.DefResource _ _ r _ ->
    normalisationError currentPass (pretty r <+> "declaration")

  V.DefPostulate{} ->
    normalisationError currentPass "postulates"

  V.DefFunction p ident _ _ expr ->
    logCompilerPass MinDetail ("compilation of" <+> quotePretty ident <+> "to loss function") $ do
      let logicImplementation = implementationOf logic
      expr' <- runReaderT (compileExpr logicImplementation expr) (logic, (ident, p))
      logCompilerPassOutput ("loss-declaration " <> prettySimple expr)
      return (DefFunction (nameOf ident) expr')

type MonadCompileLoss m =
  ( MonadCompile m
  , MonadReader (DifferentiableLogic, V.DeclProvenance) m
  )

compileArg :: MonadCompileLoss m => DifferentialLogicImplementation -> V.CheckedArg -> m LExpr
compileArg t arg = compileExpr t (V.argExpr arg)

-- |Helper function for compiling Literals
compileLiteral :: DifferentialLogicImplementation -> V.Literal -> Double
compileLiteral t l = case l of
  V.LUnit{}        -> developerError "Loss Function should not encounter LUnit"
  V.LBool    True  -> compileTrue t
  V.LBool    False -> compileFalse t
  V.LIndex _ e     -> fromIntegral e
  V.LNat     e     -> fromIntegral e
  V.LInt     e     -> fromIntegral e
  V.LRat     e     -> fromRational e

-- |Helps compile a name from DBBinding, even if there is no name given
compileDBBinding :: Maybe Name -> Name
compileDBBinding = fromMaybe "No_name"

-- |Compile a property or single expression
compileExpr :: MonadCompileLoss m => DifferentialLogicImplementation -> V.CheckedExpr -> m LExpr
compileExpr t e = showExit $ do
  e' <- showEntry e
  case e' of
    -- logical operatives
    V.NotExpr p [e1]     -> case compileNot t of
      Just f  -> f <$> compileArg t e1
      Nothing -> do
        (logic, declProv) <- ask
        ne1 <- runReaderT (lowerNot (argExpr e1)) (logic, declProv, p)
        compileExpr t ne1

    V.AndExpr     _ [e1, e2] -> compileAnd t <$> compileArg t e1 <*> compileArg t e2
    V.OrExpr      _ [e1, e2] -> compileOr t <$> compileArg t e1 <*> compileArg t e2
    V.ImpliesExpr _ [e1, e2] -> compileImplies t <$> (Negation <$> compileArg t e1) <*> compileArg t e2

    -- arithmetic operations
    V.AddExpr   _ _ [e1, e2] -> Addition <$> compileArg t e1 <*> compileArg t e2
    V.SubExpr   _ _ [e1, e2] -> Subtraction <$> compileArg t e1 <*> compileArg t e2
    V.MulExpr   _ _ [e1, e2] -> Multiplication <$> compileArg t e1 <*> compileArg t e2
    V.DivExpr   _ _ [e1, e2] -> Division <$> compileArg t e1 <*> compileArg t e2
    V.NegExpr   _ _ [e1]     -> Negation <$> compileArg t e1

    V.EqualityTCExpr _ op _ _ _ _ [e1, e2] -> case op of
      V.Neq  -> compileNeq t <$> compileArg t e1 <*> compileArg t e2
      V.Eq   -> compileEq t <$> (Max (Constant 0) <$> (Subtraction <$> compileArg t e1 <*> compileArg t e2)) <*> (Max (Constant 0) <$> (Subtraction <$> compileArg t e2 <*> compileArg t e1))

    V.OrderTCExpr    _ order _ _ _ _ [e1, e2] ->
      case order of
        V.Le -> compileLe t <$> compileArg t e1 <*> compileArg t e2
        V.Lt -> compileLt t <$> compileArg t e1 <*> compileArg t e2
        V.Ge -> compileGe t <$> compileArg t e1 <*> compileArg t e2
        V.Gt -> compileGt t <$> compileArg t e1 <*> compileArg t e2

    V.VecLiteral _ _ xs                -> TensorLiteral <$> traverse (compileExpr t) xs
    V.Literal _ l                      -> return $ Constant $ compileLiteral t l
    V.App _ (V.Var _ (V.Free ident)) p -> NetworkApplication (V.nameOf ident) <$> traverse (compileArg t) p
    V.Var _ (V.Bound var)              -> return (Variable (V.dbIndex var))
    V.AtExpr _ _ _ [xs, i]             -> At <$> compileArg t xs <*> compileArg t i
    V.Let _ x binder expression          -> Let (compileDBBinding (V.binderRepresentation binder)) <$> compileExpr t x <*> compileExpr t expression
    V.Lam _ binder x                     -> Lambda (compileDBBinding (V.binderRepresentation binder)) <$> compileExpr t x

    V.QuantifierTCExpr _ q binder body         -> do
      body' <- compileExpr t body
      let varName = V.getBinderName binder
      return $ Quantifier (compileQuant q) varName (Domain ()) body'

    V.Hole{}     -> resolutionError currentPass "Should not enounter Hole"
    V.Meta{}     -> resolutionError currentPass "Should not enounter Meta"
    V.Ann{}      -> normalisationError currentPass "Should not enounter Ann"
    V.Pi{}       -> unexpectedTypeInExprError currentPass "Should not enounter Pi"
    V.Universe{} -> unexpectedTypeInExprError currentPass "Should not enounter Universe"
    V.IfExpr{}   -> unexpectedExprError currentPass "If statements are not handled at the moment (possibly in the future)"
    _            -> unexpectedExprError currentPass (prettyVerbose e)

compileQuant :: V.Quantifier -> Quantifier
compileQuant V.Forall = All
compileQuant V.Exists = Any

--------------------------------------------------------------------------------
-- Compilation

type MonadLowerNot m =
  ( MonadCompile m
  , MonadReader (DifferentiableLogic, V.DeclProvenance, V.Provenance) m
  )

lowerNot :: MonadLowerNot m => V.CheckedExpr -> m V.CheckedExpr
lowerNot arg = case arg of

  ----------------
  -- Base cases --

  -- Literals
  V.BoolLiteral    p b                    -> return $ V.BoolLiteral p (not b)
  -- Various forms of order and equality
  V.OrderExpr      p dom ord args         -> return $ V.OrderExpr p dom (neg ord) args
  V.EqualityExpr   p dom eq args          -> return $ V.EqualityExpr p dom (neg eq) args
  -- Order and equality expressions (note that we don't push these through the type-class
  -- solution as these will never be compiled.)
  V.OrderTCExpr    p ord t1 t2 t3 s args  -> return $ V.OrderTCExpr p (neg ord) t1 t2 t3 s args
  V.EqualityTCExpr p eq t1 t2 t3 s args   -> return $ V.EqualityTCExpr p (neg eq) t1 t2 t3 s args

  -- Double negation
  V.NotExpr       _ [e]                   -> return $ argExpr e

  ---------------------
  -- Inductive cases --

  V.ForallRatExpr p binder body  -> V.ExistsRatExpr p binder <$> lowerNot body
  V.ExistsRatExpr p binder body  -> V.ForallRatExpr p binder <$> lowerNot body
  V.ImpliesExpr   p [e1, e2]     -> do
    ne2 <- lowerNotArg e2
    return $ V.AndExpr p [e1, ne2]
  V.OrExpr        p args         -> V.AndExpr p <$> traverse lowerNotArg args
  V.AndExpr       p args         -> V.OrExpr  p <$> traverse lowerNotArg args
  V.IfExpr p tRes [c, e1, e2]    -> do
    ne1 <- lowerNotArg e1
    ne2 <- lowerNotArg e2
    return $ V.IfExpr p tRes [c, ne1, ne2]

  -- Errors
  e  -> do
    (logic, declProv, notProv) <- ask
    throwError $ UnsupportedNegatedOperation logic declProv notProv e

lowerNotArg :: MonadLowerNot m => V.CheckedArg -> m V.CheckedArg
lowerNotArg = traverse lowerNot

----------------------------------------------------------------------------------------------------
-- Handling normalisation options

normalisationOptions :: NormalisationOptions
normalisationOptions = Options
  { declContext                 = mempty
  , boundContext                = mempty
  , normaliseDeclApplications   = False
  , normaliseLambdaApplications = False
  , normaliseStdLibApplications = False
  , normaliseBuiltin            = normBuiltin
  }

normBuiltin :: V.Builtin -> Bool
normBuiltin b = case b of
  V.TypeClassOp t -> case t of
    V.FromNatTC{} -> True
    V.FromRatTC   -> True
    V.FromVecTC{} -> True
    V.NotTC       -> True
    V.AndTC       -> True
    V.OrTC        -> True
    V.ImpliesTC   -> True
    V.MapTC       -> True
    V.NegTC       -> True
    V.AddTC       -> True
    V.SubTC       -> True
    V.MulTC       -> True
    V.DivTC       -> True

    _             -> False

  V.FromNat{}      -> True
  V.FromRat{}      -> True
  V.FromVec{}      -> True
  V.Foreach{}      -> True

  _                -> False

-----------------------------------------------------------------------
-- Debugging options

showEntry :: MonadCompile m => V.CheckedExpr -> m V.CheckedExpr
showEntry e = do
  logDebug MinDetail ("loss-entry " <> prettySimple e)
  incrCallDepth
  return e

showExit :: MonadCompile m => m LExpr -> m LExpr
showExit mNew = do
  new <- mNew
  decrCallDepth
  logDebug MinDetail ("loss-exit " <+> pretty (show new))
  return new
