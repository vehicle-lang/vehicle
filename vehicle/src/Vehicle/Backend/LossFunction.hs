module Vehicle.Backend.LossFunction
  ( module X,
    convertLossExpr,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Vehicle.Backend.LossFunction.Compile as X
import Vehicle.Backend.LossFunction.Logics (DifferentialLogicImplementation (..), NotTranslation (..))
import Vehicle.Backend.Tensors.Convert
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (evalApp, normEval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.DSL (DSLExpr, fromDSL)
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Builtin

convertLossExpr ::
  forall m.
  (MonadTensorProperty m) =>
  DifferentialLogicImplementation ->
  NFValue Builtin ->
  m (NFValue Builtin)
convertLossExpr DifferentialLogicImplementation {..} = go
  where
    go :: NFValue Builtin -> m (NFValue Builtin)
    go expr = case expr of
      VMeta {} -> unexpectedExprError currentPass "VMeta"
      VUniverse {} -> unexpectedExprError currentPass "VUniverse"
      VPi {} -> unexpectedExprError currentPass "VPi"
      VFreeVar v spine -> VFreeVar v <$> traverse (traverse go) spine
      VBoundVar v spine -> VBoundVar v <$> traverse (traverse go) spine
      VBuiltin b spine -> do
        case goBuiltin b of
          NoTranslation -> do
            spine' <- traverse (traverse go) spine
            return $ VBuiltin b spine'
          Translation t -> do
            spine' <- traverse (traverse go) spine
            let unnormFn = fromDSL mempty t
            normFn <- _
            evalApp <- evalApp _ _ _
            normEval opts
          EliminateNot -> do
            spine' <- traverse (traverse go) spine
            case spine' of
              [arg] -> case lowerNot (argExpr arg) of
                Right result -> return result
                Left errExpr -> throwError $ UnsupportedNegatedOperation logicID errExpr
              _ -> illTypedError currentPass (prettyVerbose expr)
          ErrorCase mkError -> do
            declProv <- ask
            throwError $ mkError declProv mempty
      VLam binder (NFBody body) -> do
        binder' <- traverse go binder
        body' <- go body
        return $ VLam binder' (NFBody body')

    goBuiltin :: Builtin -> BuiltinTranslation
    goBuiltin b = case b of
      BuiltinConstructor c -> goConstructor c
      BuiltinFunction f -> goFunction f
      _ -> NoTranslation

    goConstructor :: BuiltinConstructor -> BuiltinTranslation
    goConstructor c = case c of
      LBool True -> Translation compileTrue
      LBool False -> Translation compileFalse
      _ -> NoTranslation

    goFunction :: BuiltinFunction -> BuiltinTranslation
    goFunction f = case f of
      And -> Translation compileAnd
      Or -> Translation compileOr
      Not -> case compileNot of
        UnaryNot func -> Translation func
        TryToEliminate -> EliminateNot
      Implies -> Translation compileImplies
      Quantifier Forall -> Translation compileForall
      Quantifier Exists -> Translation compileExists
      If -> ErrorCase UnsupportedIfOperation
      _ -> NoTranslation

normEvalApp ::
  (MonadTensorProperty m) =>
  WHNFValue Builtin ->
  NFSpine Builtin ->
  m (NFValue Builtin)
normEvalApp fun [] = normEval opts fun
normEvalApp fun args@(a : as) = case fun of
  VLam binder (WHNFBody env body)
    | not (visibilityMatches binder a) ->
        visibilityError currentPass (prettyVerbose fun) (prettyVerbose args)
    | otherwise -> do
        let newEnv = extendEnvWithDefined (argExpr a) binder env
        body' <- normEval opts newEnv body
        case as of
          [] -> return body'
          (b : bs) -> normEvalApp body' (b : bs)
  VBuiltin b spine -> evalBuiltin (evalApp opts) b (spine <> args)
  _ -> unexpectedExprError currentPass (prettyVerbose fun)

lowerNot :: NFValue Builtin -> Either (NFValue Builtin) (NFValue Builtin)
lowerNot arg = case arg of
  -- Base cases
  VBoolLiteral v ->
    return $ VBoolLiteral (not v)
  VBuiltinFunction (Order dom op) args ->
    return $ VBuiltinFunction (Order dom (neg op)) args
  VBuiltinFunction (Equals dom op) args ->
    return $ VBuiltinFunction (Equals dom (neg op)) args
  VBuiltinFunction Not [x] ->
    return $ argExpr x
  -- Inductive cases
  VInfiniteQuantifier q args binder (NFBody body) -> do
    negBody <- lowerNot body
    return $ VInfiniteQuantifier (neg q) args binder (NFBody negBody)
  VBuiltinFunction And args@[_, _] -> do
    nargs <- traverse (traverse lowerNot) args
    return $ VBuiltinFunction Or nargs
  VBuiltinFunction Or args@[_, _] -> do
    nargs <- traverse (traverse lowerNot) args
    return $ VBuiltinFunction And nargs
  VBuiltinFunction If [tRes, c, e1, e2] -> do
    ne1 <- traverse lowerNot e1
    ne2 <- traverse lowerNot e2
    return $ VBuiltinFunction If [tRes, c, ne1, ne2]
  -- Error cases
  _ -> Left arg

{-
    -- Inductive cases

    -- Errors
    e -> throwError $ UnsupportedNegatedOperation logic notProv e
    _ -> throwError $ UnsupportedNegatedOperation (logicID logic) notProv
-}
data BuiltinTranslation
  = NoTranslation
  | Translation (DSLExpr Builtin)
  | EliminateNot
  | ErrorCase (DeclProvenance -> Provenance -> CompileError)

currentPass :: CompilerPass
currentPass = "loss function translation"
