module Vehicle.Backend.LossFunction.Compile
  ( compile,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Vehicle.Backend.LossFunction.Logics
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude (BoundCtx, HasName (..), visibilityMatches)
import Vehicle.Compile.Prelude qualified as V
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard qualified as V
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Compile.Type.Subsystem.Standard.Interface qualified as V
import Vehicle.Compile.Type.Subsystem.Standard.Patterns qualified as V
import Vehicle.Expr.DSL
import Vehicle.Expr.DeBruijn (Ix, substDBInto)
import Vehicle.Libraries.StandardLibrary
import Vehicle.Prelude
import Vehicle.Syntax.AST (argExpr)

--------------------------------------------------------------------------------
-- Compilation

-- | The translation into the LExpr (this is the exported top compile function)
compile ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  V.Prog Ix V.StandardBuiltin ->
  m (V.Prog Ix V.StandardBuiltin)
compile logic typedProg = do
  let logicImplementation = implementationOf logic
  reformattedProg <- reformatLogicalOperators logicImplementation typedProg
  lossProg <- runReaderT (compileProg logicImplementation reformattedProg) mempty
  return lossProg

--------------------------------------------------------------------------------
-- Utilities

currentPass :: Doc a
currentPass = "compilation to loss functions"

--------------------------------------------------------------------------------
-- Main compilation pass

type MonadLoss m =
  ( MonadCompile m,
    MonadReader (BoundCtx V.StandardBinder) m
  )

compileProg ::
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  V.StandardProg ->
  m V.StandardProg
compileProg logic (V.Main ds) =
  logCompilerPass MinDetail currentPass $
    V.Main <$> traverse (compileDecl logic) ds

compileDecl ::
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  V.StandardDecl ->
  m V.StandardDecl
compileDecl logic decl = do
  let declProv = (V.identifierOf decl, V.provenanceOf decl)
  let sectionText = "compilation of" <+> quotePretty (V.identifierOf decl) <+> "to loss function"
  logCompilerPass MinDetail sectionText $ do
    result <- traverse (compileExpr logic declProv) decl
    logDebug MaxDetail $ prettyFriendly result
    return result

compileExpr ::
  forall m.
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  V.DeclProvenance ->
  V.StandardExpr ->
  m V.StandardExpr
compileExpr logic declProv = go
  where
    go :: V.StandardExpr -> m V.StandardExpr
    go = \case
      V.Hole {} -> resolutionError currentPass "Hole"
      V.Meta {} -> resolutionError currentPass "Meta"
      V.Ann p e t -> V.Ann p <$> go e <*> go t
      V.Universe p l -> return $ V.Universe p l
      V.Builtin p b -> compileBuiltin logic declProv p p b []
      V.App p1 (V.Builtin p2 b) args -> do
        args' <- traverse goArg (NonEmpty.toList args)
        compileBuiltin logic declProv p1 p2 b args'
      V.BoundVar p v -> return $ V.BoundVar p v
      V.FreeVar p v -> return $ V.FreeVar p v
      V.App p fun args -> do
        fun' <- go fun
        args' <- traverse goArg args
        return $ V.App p fun' args'
      V.Pi p binder body -> do
        binder' <- goBinder binder
        body' <- underBinder binder' (go body)
        return $ V.Pi p binder' body'
      V.Lam p binder body -> do
        binder' <- goBinder binder
        body' <- underBinder binder' (go body)
        return $ V.Lam p binder' body'
      V.Let p bound binder body -> do
        bound' <- go bound
        binder' <- goBinder binder
        body' <- underBinder binder' (go body)
        return $ V.Let p bound' binder' body'

    goArg :: V.StandardArg -> m V.StandardArg
    goArg arg
      -- Need to filter instance arguments otherwise we visit quantifiers twice,
      -- once for the type-class op and once for the instantiation.
      | V.isInstance arg = return arg
      | otherwise = traverse go arg

    goBinder :: V.StandardBinder -> m V.StandardBinder
    goBinder = traverse go

    underBinder :: V.StandardBinder -> m a -> m a
    underBinder binder = local (binder :)

compileBuiltin ::
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  V.DeclProvenance ->
  V.BuiltinUpdate m Ix V.StandardBuiltin V.StandardBuiltin
compileBuiltin logic@DifferentialLogicImplementation {..} declProv p1 p2 op args = do
  let originalExpr = V.normAppList p1 (V.Builtin p2 op) args
  maybeLossOp <- case op of
    V.BuiltinConstructor x -> return $ case x of
      V.LBool True -> Just $ Left compileTrue
      V.LBool False -> Just $ Left compileFalse
      _ -> Nothing
    V.BuiltinFunction x -> case x of
      V.Not -> case compileNot of
        TryToEliminate ->
          compilerDeveloperError
            "Should have eliminated `not` already in compilation to loss functions"
        UnaryNot notFn -> return $ Just $ Left notFn
      V.And -> case compileAnd of
        NaryAnd andFn -> return $ Just $ Left andFn
        BinaryAnd andFn -> return $ Just $ Left andFn
      V.Or -> case compileOr of
        NaryOr orFn -> return $ Just $ Left orFn
        BinaryOr orFn -> return $ Just $ Left orFn
      V.Implies -> return $ Just $ Left compileImplies
      V.Quantifier q -> compileQuantifier logic q args
      V.If -> throwError $ UnsupportedIfOperation declProv p2
      -- TODO really not safe to throw away the type information here, but
      -- in the short term it might work.
      V.Equals V.EqRat V.Eq -> return $ Just $ Left compileEq
      V.Equals _ V.Eq -> return $ Just $ Right $ castToBool logic originalExpr
      V.Equals V.EqRat V.Neq -> return $ Just $ Left compileNeq
      V.Equals _ V.Neq -> return $ Just $ Right $ castToBool logic originalExpr
      V.Order V.OrderRat V.Le -> return $ Just $ Left compileLe
      V.Order _ V.Le -> return $ Just $ Right $ castToBool logic originalExpr
      V.Order V.OrderRat V.Lt -> return $ Just $ Left compileLt
      V.Order _ V.Lt -> return $ Just $ Right $ castToBool logic originalExpr
      V.Order V.OrderRat V.Ge -> return $ Just $ Left compileGe
      V.Order _ V.Ge -> return $ Just $ Right $ castToBool logic originalExpr
      V.Order V.OrderRat V.Gt -> return $ Just $ Left compileGt
      V.Order _ V.Gt -> return $ Just $ Right $ castToBool logic originalExpr
      _ -> return Nothing
    V.BuiltinType x -> return $ case x of
      V.Bool -> Just $ Left compileBool
      _ -> Nothing
    V.TypeClass {} -> return Nothing
    V.TypeClassOp tc -> case tc of
      V.QuantifierTC q -> compileQuantifier logic q args
      V.EqualsTC V.Eq -> return $ Just $ Left compileEq
      V.EqualsTC V.Neq -> return $ Just $ Left compileNeq
      V.OrderTC V.Le -> return $ Just $ Left compileLe
      V.OrderTC V.Lt -> return $ Just $ Left compileLt
      V.OrderTC V.Ge -> return $ Just $ Left compileGe
      V.OrderTC V.Gt -> return $ Just $ Left compileGt
      _ -> return Nothing
    V.NatInDomainConstraint -> return Nothing

  case maybeLossOp of
    Nothing -> return originalExpr
    Just (Left lossOp) -> substArgsThrough p1 op (fromDSL p2 lossOp) args
    Just (Right lossExpr) -> return $ fromDSL p2 lossExpr

-- | We perform a tiny bit of subsitution to tidy up the expression afterwards.
substArgsThrough ::
  forall m.
  (MonadLoss m) =>
  V.Provenance ->
  V.StandardBuiltin ->
  V.Expr Ix V.StandardBuiltin ->
  [V.Arg Ix V.StandardBuiltin] ->
  m (V.Expr Ix V.StandardBuiltin)
substArgsThrough p originalFun initialFun initialArgs = go initialFun initialArgs
  where
    go ::
      V.Expr Ix V.StandardBuiltin ->
      [V.Arg Ix V.StandardBuiltin] ->
      m (V.Expr Ix V.StandardBuiltin)
    go fun args = case (fun, filter V.isExplicit args) of
      (V.Lam _ binder body, a : as)
        | visibilityMatches binder a -> go (argExpr a `substDBInto` body) as
        | otherwise ->
            compilerDeveloperError $
              "Loss function substitution not well-typed"
                <> line
                <> indent
                  2
                  ( "original op:" <+> pretty originalFun
                      <> line
                      <> "loss op:" <+> prettyVerbose initialFun
                      <> line
                      <> "arguments:" <+> prettyVerbose initialArgs
                  )
      _ -> return $ V.normAppList p fun args

castToBool :: DifferentialLogicImplementation -> V.Expr Ix V.StandardBuiltin -> V.StandardDSLExpr
castToBool logic x = builtinFunction V.If @@ [toDSL x, compileTrue logic, compileFalse logic]

compileQuantifier ::
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  V.Quantifier ->
  [V.StandardArg] ->
  m (Maybe (Either V.StandardDSLExpr V.StandardDSLExpr))
compileQuantifier logic q args = case reverse args of
  V.ExplicitArg _ _ (V.Lam _ binder _) : _ -> do
    ctx <- ask
    let typ = V.typeOf binder
    let names = fmap (fromMaybe "<no-name>" . nameOf) ctx
    return $ Just $ Left $ case q of
      V.Forall -> compileForall logic typ (V.getBinderName binder) names
      V.Exists -> compileExists logic typ (V.getBinderName binder) names
  _ -> unexpectedExprError currentPass (pretty q <+> "@" <+> prettyVerbose args)

--------------------------------------------------------------------------------
-- Reformating logical operators

reformatLogicalOperators ::
  forall m.
  (MonadCompile m) =>
  DifferentialLogicImplementation ->
  V.Prog Ix V.StandardBuiltin ->
  m (V.Prog Ix V.StandardBuiltin)
reformatLogicalOperators logic = traverse (V.traverseBuiltinsM builtinUpdateFunction)
  where
    builtinUpdateFunction :: V.BuiltinUpdate m Ix V.StandardBuiltin V.StandardBuiltin
    builtinUpdateFunction p1 p2 b args = do
      maybeUpdatedExpr <- case b of
        V.BuiltinFunction V.Not -> case compileNot logic of
          TryToEliminate -> Just <$> lowerNot p2 (argExpr $ head args)
          UnaryNot {} -> return Nothing
        V.BuiltinFunction V.And -> case compileAnd logic of
          NaryAnd {} -> return $ Just (V.AndExpr p1 (flattenAnds (V.RelevantExplicitArg p1 (V.AndExpr p1 (NonEmpty.fromList args)))))
          BinaryAnd {} -> return Nothing
        V.BuiltinFunction V.Or -> case compileOr logic of
          NaryOr {} -> return $ Just (V.OrExpr p1 (flattenOrs (V.RelevantExplicitArg p1 (V.OrExpr p1 (NonEmpty.fromList args)))))
          BinaryOr {} -> return Nothing
        _ -> return Nothing

      let unchangedExpr = V.normAppList p1 (V.Builtin p2 b) args
      return $ fromMaybe unchangedExpr maybeUpdatedExpr

    lowerNot :: V.Provenance -> V.Expr Ix V.StandardBuiltin -> m (V.Expr Ix V.StandardBuiltin)
    lowerNot notProv arg = case arg of
      -- Base cases
      V.BoolLiteral p b -> return $ V.BoolLiteral p (not b)
      V.OrderExpr p dom ord args -> return $ V.OrderExpr p dom (neg ord) args
      V.EqualityExpr p dom eq args -> return $ V.EqualityExpr p dom (neg eq) args
      V.NotExpr _ [e] -> return $ argExpr e
      -- Inductive cases
      V.ForallExpr p binder body -> V.ExistsExpr p binder <$> lowerNot notProv body
      V.ExistsExpr p binder body -> V.ForallExpr p binder <$> lowerNot notProv body
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
      _ -> throwError $ UnsupportedNegatedOperation (logicID logic) notProv

    flattenAnds :: V.Arg Ix V.StandardBuiltin -> NonEmpty (V.Arg Ix V.StandardBuiltin)
    flattenAnds arg = case argExpr arg of
      V.AndExpr _ [e1, e2] -> flattenAnds e1 <> flattenAnds e2
      _ -> [arg]
    flattenOrs :: V.Arg Ix V.StandardBuiltin -> NonEmpty (V.Arg Ix V.StandardBuiltin)
    flattenOrs arg = case argExpr arg of
      V.OrExpr _ [e1, e2] -> flattenOrs e1 <> flattenOrs e2
      _ -> [arg]
