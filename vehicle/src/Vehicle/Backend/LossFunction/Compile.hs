module Vehicle.Backend.LossFunction.Compile
  ( compile,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (fromMaybe)
import Vehicle.Backend.LossFunction.Logics
import Vehicle.Backend.LossFunction.TypeSystem
import Vehicle.Backend.LossFunction.TypeSystem.BuiltinInstances (lossBuiltinInstances)
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem (resolveInstanceArguments, typeCheckWithSubsystem)
import Vehicle.Compile.Type.Subsystem.Standard.Core (StandardBuiltin)
import Vehicle.Compile.Type.Subsystem.Standard.Core qualified as S
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.DeBruijn (Ix)
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary

--------------------------------------------------------------------------------
-- Compilation

-- | The translation into the LExpr (this is the exported top compile function)
compile ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  Prog Ix StandardBuiltin ->
  m (Prog Ix LossBuiltin)
compile logic typedProg =
  logCompilerPass MinDetail currentPass $ do
    let logicImplementation = implementationOf logic

    -- Some logics require some preprocessing (e.g. DL2 requires that negation
    -- is pushed all the way in.)
    reformattedProg <- preprocessLogicalOperators logicImplementation typedProg

    let instanceCandidates = lossBuiltinInstances logicImplementation
    lossProgWithInstances <- typeCheckWithSubsystem instanceCandidates reformattedProg

    lossProg <- resolveInstanceArguments (fmap unnormalised lossProgWithInstances)

    return lossProg

--------------------------------------------------------------------------------
-- Utilities

currentPass :: Doc a
currentPass = "compilation to loss functions"

--------------------------------------------------------------------------------
-- Main compilation pass
{-
type MonadLoss m =
  ( MonadCompile m,
    MonadReader (BoundCtx StandardBinder) m
  )

compileProg ::
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  StandardProg ->
  m StandardProg
compileProg logic (Main ds) =
    Main <$> traverse (compileDecl logic) ds

compileExpr ::
  forall m.
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  DeclProvenance ->
  StandardExpr ->
  m StandardExpr
compileExpr logic declProv = go
  where
    go :: StandardExpr -> m StandardExpr
    go = \case
      Hole {} -> resolutionError currentPass "Hole"
      Meta {} -> resolutionError currentPass "Meta"
      Ann p e t -> Ann p <$> go e <*> go t
      Universe p l -> return $ Universe p l
      Builtin p b -> compileBuiltin logic declProv p p b []
      App p1 (Builtin p2 b) args -> do
        args' <- traverse goArg (NonEmpty.toList args)
        compileBuiltin logic declProv p1 p2 b args'
      BoundVar p v -> return $ BoundVar p v
      FreeVar p v -> return $ FreeVar p v
      App p fun args -> do
        fun' <- go fun
        args' <- traverse goArg args
        return $ App p fun' args'
      Pi p binder body -> do
        binder' <- goBinder binder
        body' <- underBinder binder' (go body)
        return $ Pi p binder' body'
      Lam p binder body -> do
        binder' <- goBinder binder
        body' <- underBinder binder' (go body)
        return $ Lam p binder' body'
      Let p bound binder body -> do
        bound' <- go bound
        binder' <- goBinder binder
        body' <- underBinder binder' (go body)
        return $ Let p bound' binder' body'

    goArg :: StandardArg -> m StandardArg
    goArg arg
      -- Need to filter instance arguments otherwise we visit quantifiers twice,
      -- once for the type-class op and once for the instantiation.
      | isInstance arg = return arg
      | otherwise = traverse go arg

    goBinder :: StandardBinder -> m StandardBinder
    goBinder = traverse go

    underBinder :: StandardBinder -> m a -> m a
    underBinder binder = local (binder :)

compileBuiltin ::
  (MonadLoss m) =>
  DifferentialLogicImplementation ->
  DeclProvenance ->
  Provenance ->
  Provenance ->
  StandardBuiltin ->
  [StandardArg] ->
  m StandardExpr
compileBuiltin logic@DifferentialLogicImplementation {..} declProv p1 p2 op args = do
  let originalExpr = normAppList p1 (Builtin p2 op) args
  maybeLossOp <- case op of
    BuiltinConstructor x -> return $ case x of
      LBool True -> Just $ Left compileTrue
      LBool False -> Just $ Left compileFalse
      _ -> Nothing
    BuiltinFunction x -> case x of
      Not -> case compileNot of
        TryToEliminate ->
          compilerDeveloperError
            "Should have eliminated `not` already in compilation to loss functions"
        UnaryNot notFn -> return $ Just $ Left notFn
      And -> case compileAnd of
        NaryAnd andFn -> return $ Just $ Left andFn
        BinaryAnd andFn -> return $ Just $ Left andFn
      Or -> case compileOr of
        NaryOr orFn -> return $ Just $ Left orFn
        BinaryOr orFn -> return $ Just $ Left orFn
      Implies -> return $ Just $ Left compileImplies
      Quantifier q -> compileQuantifier logic q args
      If -> throwError $ UnsupportedIfOperation declProv p2
      -- TODO really not safe to throw away the type information here, but
      -- in the short term it might work.
      Equals EqRat Eq -> return $ Just $ Left compileEq
      Equals _ Eq -> return $ Just $ Right $ castToBool logic originalExpr
      Equals EqRat Neq -> return $ Just $ Left compileNeq
      Equals _ Neq -> return $ Just $ Right $ castToBool logic originalExpr
      Order OrderRat Le -> return $ Just $ Left compileLe
      Order _ Le -> return $ Just $ Right $ castToBool logic originalExpr
      Order OrderRat Lt -> return $ Just $ Left compileLt
      Order _ Lt -> return $ Just $ Right $ castToBool logic originalExpr
      Order OrderRat Ge -> return $ Just $ Left compileGe
      Order _ Ge -> return $ Just $ Right $ castToBool logic originalExpr
      Order OrderRat Gt -> return $ Just $ Left compileGt
      Order _ Gt -> return $ Just $ Right $ castToBool logic originalExpr
      _ -> return Nothing
    BuiltinType x -> return $ case x of
      Bool -> Just $ Left compileBool
      _ -> Nothing
    TypeClass {} -> return Nothing
    TypeClassOp tc -> case tc of
      QuantifierTC q -> compileQuantifier logic q args
      EqualsTC Eq -> return $ Just $ Left compileEq
      EqualsTC Neq -> return $ Just $ Left compileNeq
      OrderTC Le -> return $ Just $ Left compileLe
      OrderTC Lt -> return $ Just $ Left compileLt
      OrderTC Ge -> return $ Just $ Left compileGe
      OrderTC Gt -> return $ Just $ Left compileGt
      _ -> return Nothing
    NatInDomainConstraint -> return Nothing

  case maybeLossOp of
    Nothing -> return originalExpr
    Just (Left lossOp) -> substArgsThrough p1 op (fromDSL p2 lossOp) args
    Just (Right lossExpr) -> return $ fromDSL p2 lossExpr

-- | We perform a tiny bit of subsitution to tidy up the expression afterwards.
substArgsThrough ::
  forall m.
  (MonadLoss m) =>
  Provenance ->
  StandardBuiltin ->
  Expr Ix StandardBuiltin ->
  [StandardArg] ->
  m (Expr Ix StandardBuiltin)
substArgsThrough p originalFun initialFun initialArgs = go initialFun initialArgs
  where
    go ::
      Expr Ix StandardBuiltin ->
      [StandardArg] ->
      m (Expr Ix StandardBuiltin)
    go fun args = case (fun, filter isExplicit args) of
      (Lam _ binder body, a : as)
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
      _ -> return $ normAppList p fun args

castToBool :: DifferentialLogicImplementation -> Expr Ix StandardBuiltin -> StandardDSLExpr
castToBool logic x = builtinFunction If @@ [toDSL x, compileTrue logic, compileFalse logic]
-}
--------------------------------------------------------------------------------
-- Preprocessing logical operators

preprocessLogicalOperators ::
  forall m.
  (MonadCompile m) =>
  DifferentialLogicImplementation ->
  Prog Ix StandardBuiltin ->
  m (Prog Ix StandardBuiltin)
preprocessLogicalOperators logic = traverse (traverseBuiltinsM builtinUpdateFunction)
  where
    builtinUpdateFunction :: BuiltinUpdate m Ix StandardBuiltin StandardBuiltin
    builtinUpdateFunction p1 p2 b args = do
      maybeUpdatedExpr <- case b of
        S.BuiltinFunction S.Not -> case compileNot logic of
          TryToEliminate -> Just <$> lowerNot p2 (argExpr $ head args)
          UnaryNot {} -> return Nothing
        _ -> return Nothing

      let unchangedExpr = normAppList p1 (Builtin p2 b) args
      return $ fromMaybe unchangedExpr maybeUpdatedExpr

    lowerNot :: Provenance -> Expr Ix StandardBuiltin -> m (Expr Ix StandardBuiltin)
    lowerNot notProv arg = case arg of
      -- Base cases
      BoolLiteral p b -> return $ BoolLiteral p (not b)
      OrderExpr p dom ord args -> return $ OrderExpr p dom (neg ord) args
      EqualityExpr p dom eq args -> return $ EqualityExpr p dom (neg eq) args
      NotExpr _ [e] -> return $ argExpr e
      -- Inductive cases
      ForallExpr p binder body -> ExistsExpr p binder <$> lowerNot notProv body
      ExistsExpr p binder body -> ForallExpr p binder <$> lowerNot notProv body
      ImpliesExpr p [e1, e2] -> do
        ne2 <- traverse (lowerNot notProv) e2
        return $ AndExpr p [e1, ne2]
      OrExpr p args -> AndExpr p <$> traverse (traverse (lowerNot notProv)) args
      AndExpr p args -> OrExpr p <$> traverse (traverse (lowerNot notProv)) args
      IfExpr p tRes [c, e1, e2] -> do
        ne1 <- traverse (lowerNot notProv) e1
        ne2 <- traverse (lowerNot notProv) e2
        return $ IfExpr p tRes [c, ne1, ne2]
      App p1 (FreeVar p2 ident) args
        | ident == identifierOf StdEqualsVector -> return $ App p1 (FreeVar p2 (identifierOf StdNotEqualsVector)) args
        | ident == identifierOf StdNotEqualsVector -> return $ App p1 (FreeVar p2 (identifierOf StdEqualsVector)) args
      -- Errors
      _ -> throwError $ UnsupportedNegatedOperation (logicID logic) notProv
