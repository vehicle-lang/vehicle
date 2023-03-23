module Vehicle.Backend.LossFunction.Compile
  ( LDecl,
    DifferentiableLogic,
    compile,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Vehicle.Backend.LossFunction.Logics
  ( DifferentialLogicImplementation (..),
    Domain (..),
    LDecl (..),
    LExpr (..),
    Quantifier (..),
    implementationOf,
  )
import Vehicle.Backend.Prelude (DifferentiableLogic (..))
import Vehicle.Compile.Descope (DescopeNamed (descopeNamed))
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Queries.LinearityAndPolarityErrors (resolveInstanceArguments)
import Vehicle.Compile.Resource qualified as V
import Vehicle.Compile.Type.Subsystem.Standard qualified as V
import Vehicle.Compile.Type.Subsystem.Standard.Patterns qualified as V
import Vehicle.Expr.Normalisable qualified as V
import Vehicle.Expr.Normalised (GluedExpr (..))
import Vehicle.Libraries.StandardLibrary
import Vehicle.Prelude
import Vehicle.Resource (Resources (..))
import Vehicle.Syntax.AST (HasName (nameOf), argExpr)

--------------------------------------------------------------------------------
-- Compilation

-- | The translation into the LExpr (this is the exported top compile function)
compile ::
  (MonadIO m, MonadCompile m) =>
  Resources ->
  DifferentiableLogic ->
  V.StandardGluedProg ->
  m [LDecl]
compile resources logic typedProg = do
  (networkCtx, expandedProg) <- expandResources resources typedProg
  let unnormalisedProg = fmap unnormalised expandedProg
  progWithoutInstanceArgs <- resolveInstanceArguments unnormalisedProg
  let descopedProg = descopeNamed progWithoutInstanceArgs
  possiblyNotNormalisedProg <- case compileNot (implementationOf logic) of
    Just {} -> return descopedProg
    Nothing -> lowerNots logic descopedProg

  compileProg networkCtx logic possiblyNotNormalisedProg

--------------------------------------------------------------------------------
-- Utilities

currentPass :: Doc a
currentPass = "compilation to loss functions"

type InputProg = V.NamedProg V.StandardBuiltin

type InputDecl = V.NamedDecl V.StandardBuiltin

type InputExpr = V.NamedExpr V.StandardBuiltin

type InputArg = V.NamedArg V.StandardBuiltin

--------------------------------------------------------------------------------
-- Main compilation pass

-- | Compile entire specification (calls compileDecl)
compileProg ::
  MonadCompile m =>
  V.NetworkContext ->
  DifferentiableLogic ->
  InputProg ->
  m [LDecl]
compileProg networkCtx logic (V.Main ds) =
  logCompilerPass MinDetail "compilation to loss function" $
    traverse (compileDecl networkCtx logic) ds

-- | Compile all functions found in spec, save their names (call compileExpr on each)
compileDecl ::
  MonadCompile m =>
  V.NetworkContext ->
  DifferentiableLogic ->
  InputDecl ->
  m LDecl
compileDecl networkCtx logic = \case
  V.DefResource _ _ r _ ->
    normalisationError currentPass (pretty r <+> "declaration")
  V.DefPostulate {} ->
    normalisationError currentPass "postulates"
  V.DefFunction p ident _ _ expr ->
    logCompilerPass MinDetail ("compilation of" <+> quotePretty ident <+> "to loss function") $ do
      let logicImplementation = implementationOf logic
      expr' <- runReaderT (compileExpr logicImplementation expr) (networkCtx, logic, (ident, p))
      return (DefFunction (nameOf ident) expr')

type MonadCompileLoss m =
  ( MonadCompile m,
    MonadReader (V.NetworkContext, DifferentiableLogic, V.DeclProvenance) m
  )

compileArg :: MonadCompileLoss m => DifferentialLogicImplementation -> InputArg -> m LExpr
compileArg t arg = compileExpr t (V.argExpr arg)

-- | Compile a property or single expression
compileExpr :: MonadCompileLoss m => DifferentialLogicImplementation -> InputExpr -> m LExpr
compileExpr t expr = showExit $ do
  e' <- showEntry expr
  case e' of
    V.Ann _ e _ -> compileExpr t e
    V.Let _ x binder expression ->
      Let (V.getBinderName binder) <$> compileExpr t x <*> compileExpr t expression
    V.Lam _ binder body -> do
      body' <- compileExpr t body
      return $ Lambda (V.getBinderName binder) body'
    V.BoundVar _ v -> return $ Variable v
    V.FreeVar _ ident -> return $ FreeVariable (nameOf ident) []
    V.App _ fun args -> do
      case fun of
        V.FreeVar _ ident -> do
          (networkCtx, _, _) <- ask
          let name = V.nameOf ident
          if name `Map.member` networkCtx
            then do
              args' <- traverse (compileArg t) args
              return $ NetworkApplication name args'
            else case findStdLibFunction ident of
              Just fn -> compileStdLibFunction fn t args
              Nothing -> do
                args' <- traverse (compileArg t) args
                return $ FreeVariable name args'
        V.Builtin _ b -> do
          explicitArgs' <- compileExplicitArgs t args
          compileBuiltin b t explicitArgs'
        _ -> notYetSupported "Application of lambda functions"
    V.Builtin _ b ->
      compileBuiltin b t mempty
    V.Hole {} -> resolutionError currentPass "Hole"
    V.Meta {} -> resolutionError currentPass "Meta"
    V.Pi {} -> unexpectedTypeInExprError currentPass "Pi"
    V.Universe {} -> unexpectedTypeInExprError currentPass "Universe"

type CompileBuiltin =
  forall m.
  MonadCompileLoss m =>
  DifferentialLogicImplementation ->
  [LExpr] ->
  m LExpr

compileBuiltin :: V.StandardBuiltin -> CompileBuiltin
compileBuiltin b = case b of
  V.CConstructor c -> compileBuiltinConstructor c
  V.CFunction f -> compileBuiltinFunction f
  _ -> \_ _ -> unexpectedTypeInExprError currentPass ("Should not enounter" <+> pretty b)

compileBuiltinConstructor :: V.BuiltinConstructor -> CompileBuiltin
compileBuiltinConstructor c t args = case c of
  V.LUnit {} -> compilerDeveloperError "Loss Function should not encounter LUnit"
  V.LBool b -> return $ Constant $ (if b then compileTrue else compileFalse) t
  V.LIndex l -> return $ Constant $ fromIntegral l
  V.LNat l -> return $ Constant $ fromIntegral l
  V.LInt l -> return $ Constant $ fromIntegral l
  V.LRat l -> return $ Constant $ fromRational l
  V.LVec _ -> compileVecLiteral t args
  V.Nil -> notYetSupportedBuiltin $ V.CConstructor c
  V.Cons -> notYetSupportedBuiltin $ V.CConstructor c

compileBuiltinFunction :: V.BuiltinFunction -> CompileBuiltin
compileBuiltinFunction f t args = case f of
  V.FromNat {} -> compileOp1 id t args
  V.FromRat {} -> compileOp1 id t args
  -- Logical operatives
  V.And -> compileOp2 (compileAnd t) t args
  V.Or -> compileOp2 (compileOr t) t args
  V.At -> compileOp2 At t args
  V.Not -> compileNotOp t args
  V.Implies -> compileOp2 (compileImplies t) t args
  V.Quantifier q _ -> compileQuantifier q t args
  -- Arithmetic operations
  V.Neg {} -> compileOp1 Negation t args
  V.Add {} -> compileOp2 Addition t args
  V.Sub {} -> compileOp2 Subtraction t args
  V.Mul {} -> compileOp2 Multiplication t args
  V.Div {} -> compileOp2 Division t args
  -- Comparison operations
  V.Equals _ eq -> compileEquality eq t args
  V.Order _ ord -> compileOrder ord t args
  -- Container operations
  V.Indices {} -> compileOp1 Range t args
  -- Not supported
  V.Fold {} -> notYetSupportedBuiltin $ V.CFunction f
  V.ConsVector {} -> notYetSupportedBuiltin $ V.CFunction f
  V.If -> notYetSupportedBuiltin $ V.CFunction f

compileStdLibFunction ::
  MonadCompileLoss m =>
  StdLibFunction ->
  DifferentialLogicImplementation ->
  NonEmpty InputArg ->
  m LExpr
compileStdLibFunction fn t args = case fn of
  StdEqualsBool -> compileEquality V.Eq t =<< compileExplicitArgs t args
  StdNotEqualsBool -> compileEquality V.Neq t =<< compileExplicitArgs t args
  StdEqualsVector -> compileEquality V.Eq t =<< compileExplicitArgs t args
  StdNotEqualsVector -> compileEquality V.Neq t =<< compileExplicitArgs t args
  StdAddVector -> compileOp2 Addition t =<< compileExplicitArgs t args
  StdSubVector -> compileOp2 Subtraction t =<< compileExplicitArgs t args
  StdVectorToVector -> compileOp1 id t =<< compileExplicitArgs t args
  StdMapVector -> compileOp2 Map t =<< compileExplicitArgs t args
  StdMapList -> compileOp2 Map t =<< compileExplicitArgs t args
  StdForeach -> case args of
    _ :| [V.ImplicitArg _ size, V.ExplicitArg _ f] -> do
      size' <- compileExpr t size
      indices <- compileBuiltin (V.CFunction V.Indices) t [size']
      f' <- compileExpr t f
      compileOp2 Map t [f', indices]
    _ -> unexpectedExprError currentPass "partially applied binary operation"
  StdExistsIndex -> notYetSupportedStdLibFunction fn
  StdForallIndex -> notYetSupportedStdLibFunction fn
  StdExistsIn -> notYetSupportedStdLibFunction fn
  StdForallIn -> notYetSupportedStdLibFunction fn
  StdVectorToList -> notYetSupportedStdLibFunction fn
  StdTensor -> notYetSupportedStdLibFunction fn

compileVecLiteral :: CompileBuiltin
compileVecLiteral _t args = return $ TensorLiteral args

compileOp1 :: (LExpr -> LExpr) -> CompileBuiltin
compileOp1 f _t = \case
  [e] -> return $ f e
  _ -> unexpectedExprError currentPass "partially applied unary operation"

compileOp2 :: (LExpr -> LExpr -> LExpr) -> CompileBuiltin
compileOp2 f _t = \case
  [e1, e2] -> return $ f e1 e2
  _ -> unexpectedExprError currentPass "partially applied binary operation"

compileNotOp :: CompileBuiltin
compileNotOp t args = case compileNot t of
  Just notFn -> compileOp1 notFn t args
  Nothing -> unexpectedExprError currentPass "encountered 'not' that has not been lowered through expression"

compileEquality :: V.EqualityOp -> CompileBuiltin
compileEquality op t [e1, e2] = return $ case op of
  V.Neq -> compileNeq t e1 e2
  V.Eq -> compileEq t e1 e2
compileEquality _ _ _ = unexpectedExprError currentPass "partially applied equality comparison"

compileOrder :: V.OrderOp -> CompileBuiltin
compileOrder ord t args = case ord of
  V.Le -> compileOp2 (compileLe t) t args
  V.Lt -> compileOp2 (compileLt t) t args
  V.Ge -> compileOp2 (compileGe t) t args
  V.Gt -> compileOp2 (compileGt t) t args

compileQuantifier :: V.Quantifier -> CompileBuiltin
compileQuantifier q _t [Lambda varName body] =
  return $ Quantifier (compileQuant q) varName (Domain ()) body
compileQuantifier _ _ _ = unexpectedExprError currentPass "partially applied quantifier"

compileQuant :: V.Quantifier -> Quantifier
compileQuant V.Forall = All
compileQuant V.Exists = Any

compileExplicitArgs :: MonadCompileLoss m => DifferentialLogicImplementation -> NonEmpty InputArg -> m [LExpr]
compileExplicitArgs t args = do
  let explicitArgs = argExpr <$> NonEmpty.filter V.isExplicit args
  traverse (compileExpr t) explicitArgs

notYetSupported :: MonadCompile m => Doc () -> m a
notYetSupported op =
  unexpectedExprError currentPass $
    op <+> "is not handled at the moment for loss function translation."

notYetSupportedBuiltin :: MonadCompile m => V.StandardBuiltin -> m a
notYetSupportedBuiltin op = notYetSupported (quotePretty op)

notYetSupportedStdLibFunction :: MonadCompile m => StdLibFunction -> m a
notYetSupportedStdLibFunction op = notYetSupported (quotePretty op)

--------------------------------------------------------------------------------
-- Lowering nots

lowerNots ::
  forall m.
  MonadCompile m =>
  DifferentiableLogic ->
  InputProg ->
  m InputProg
lowerNots logic = traverse (V.traverseBuiltinsM builtinUpdateFunction)
  where
    builtinUpdateFunction :: V.BuiltinUpdate m V.NamedBinding V.NamedVar V.StandardBuiltin V.StandardBuiltin
    builtinUpdateFunction p1 p2 b args = case b of
      V.CFunction V.Not -> lowerNot p2 (argExpr $ head args)
      _ -> return $ V.normAppList p1 (V.Builtin p2 b) args

    lowerNot :: V.Provenance -> InputExpr -> m InputExpr
    lowerNot notProv arg = case arg of
      -- Base cases
      V.BoolLiteral p b -> return $ V.BoolLiteral p (not b)
      V.OrderExpr p dom ord args -> return $ V.OrderExpr p dom (neg ord) args
      V.EqualityExpr p dom eq args -> return $ V.EqualityExpr p dom (neg eq) args
      V.NotExpr _ [e] -> return $ argExpr e
      -- Inductive cases
      V.ForallRatExpr p binder body -> V.ExistsRatExpr p binder <$> lowerNot notProv body
      V.ExistsRatExpr p binder body -> V.ForallRatExpr p binder <$> lowerNot notProv body
      V.ImpliesExpr p [e1, e2] -> do
        ne2 <- traverse (lowerNot notProv) e2
        return $ V.AndExpr p [e1, ne2]
      V.OrExpr p args -> V.AndExpr p <$> traverse (traverse (lowerNot notProv)) args
      V.AndExpr p args -> V.OrExpr p <$> traverse (traverse (lowerNot notProv)) args
      V.IfExpr p tRes [c, e1, e2] -> do
        ne1 <- traverse (lowerNot notProv) e1
        ne2 <- traverse (lowerNot notProv) e2
        return $ V.IfExpr p tRes [c, ne1, ne2]
      V.App p1 (V.FreeVar p2 ident) args
        | ident == V.identifierOf StdEqualsVector -> return $ V.App p1 (V.FreeVar p2 (V.identifierOf StdNotEqualsVector)) args
        | ident == V.identifierOf StdNotEqualsVector -> return $ V.App p1 (V.FreeVar p2 (V.identifierOf StdEqualsVector)) args
      -- Errors
      e -> throwError $ UnsupportedNegatedOperation logic notProv e

-----------------------------------------------------------------------
-- Debugging options

showEntry :: MonadCompile m => InputExpr -> m InputExpr
showEntry e = do
  logDebug MinDetail ("loss-entry " <> prettyFriendly e)
  incrCallDepth
  return e

showExit :: MonadCompile m => m LExpr -> m LExpr
showExit mNew = do
  new <- mNew
  decrCallDepth
  logDebug MinDetail ("loss-exit " <+> pretty (show new))
  return new
