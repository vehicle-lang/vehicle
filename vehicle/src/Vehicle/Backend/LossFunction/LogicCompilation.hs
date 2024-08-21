{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.LogicCompilation
  ( compileLogic,
    convertToLossBuiltins,
    normStandardExprToLoss,
    MonadLogicCtx,
    runMonadLogicT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Vehicle.Backend.LossFunction.Core
import Vehicle.Backend.LossFunction.Core qualified as L
import Vehicle.Backend.LossFunction.Logics qualified as DSL
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Context.Bound (MonadBoundContext, getNamedBoundCtx, runFreshBoundContextT)
import Vehicle.Compile.Context.Bound.Class (MonadBoundContext (..))
import Vehicle.Compile.Context.Free (MonadFreeContext, runFreshFreeContextT)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (EvaluableClosure (..), eval, evalApp, normaliseInEnv)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Expr.Interface (pattern INot)
import Vehicle.Data.Expr.Value (VBinder, Value (..), WHNFBoundEnv, WHNFClosure (..), WHNFValue, boundContextToEnv, extendEnvWithBound, extendEnvWithDefined)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Syntax.Builtin (Builtin)
import Vehicle.Syntax.Builtin qualified as V

--------------------------------------------------------------------------------
-- Monad

type MonadLogicCtx =
  ( DifferentiableLogicID,
    Either DeclProvenance DifferentiableLogicField,
    DifferentiableLogicImplementation
  )

type MonadLogic m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadBoundContext MixedLossValue m,
    MonadReader MonadLogicCtx m
  )

runMonadLogicT ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadBoundContext MixedLossValue m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Either DeclProvenance DifferentiableLogicField ->
  ReaderT MonadLogicCtx m a ->
  m a
runMonadLogicT logicID logic origin =
  flip runReaderT (logicID, origin, logic)

getLogic :: (MonadLogic m) => m DifferentiableLogicImplementation
getLogic = do
  (_, _, logic) <- ask
  return logic

getDeclProvenance :: (MonadLogic m) => m (Either DeclProvenance DifferentiableLogicField)
getDeclProvenance = do
  (_, prov, _) <- ask
  return prov

lookupLogicField :: (MonadLogic m) => DifferentiableLogicField -> m MixedLossValue
lookupLogicField field = do
  logic <- getLogic
  case Map.lookup field logic of
    Nothing -> compilerDeveloperError $ "Non-compiled logic field" <+> quotePretty field <+> "found"
    Just value -> return value

--------------------------------------------------------------------------------
-- Logic setup

-- | Compiles a differentiable logic from the DSL for expressing them into a
-- form suitable for normalisation. Eventually the DSL should be replaced by
-- the something in the language.
compileLogic ::
  forall m.
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DSL.DifferentialLogicDSL ->
  m DifferentiableLogicImplementation
compileLogic logicID dsl = do
  logCompilerPass MidDetail ("compiling logic" <+> quotePretty logicID) $ do
    go mempty (LinkedHashMap.toList dsl)
  where
    go ::
      DifferentiableLogicImplementation ->
      [(DifferentiableLogicField, Expr Ix Builtin)] ->
      m DifferentiableLogicImplementation
    go impl [] = return impl
    go impl ((field, expr) : fields) = do
      value <-
        logCompilerSection MaxDetail ("compiling logic field" <+> quotePretty field) $
          runFreshFreeContextT (Proxy @Builtin) $
            runFreshBoundContextT (Proxy @MixedLossValue) $
              runMonadLogicT logicID impl (Right field) $ do
                mixedLossValue <- normStandardExprToLoss mempty expr
                lossValue <- transformMixedClosureToStandardClosure mixedLossValue
                transformLossClosureToMixedClosure lossValue
      go (Map.insert field value impl) fields

traverseClosures ::
  forall m closure1 closure2 builtin.
  (Monad m, MonadBoundContext (Value closure1 builtin) m) =>
  (VBinder closure1 builtin -> closure1 -> m closure2) ->
  Value closure1 builtin ->
  m (Value closure2 builtin)
traverseClosures f = go
  where
    go :: Value closure1 builtin -> m (Value closure2 builtin)
    go = \case
      VUniverse l -> return $ VUniverse l
      VMeta m spine -> VMeta m <$> traverseArgs go spine
      VFreeVar v spine -> VFreeVar v <$> traverseArgs go spine
      VBoundVar v spine -> VBoundVar v <$> traverseArgs go spine
      VBuiltin b spine -> VBuiltin b <$> traverseArgs go spine
      VPi binder body -> do
        binder' <- traverse go binder
        body' <- addBinderToContext binder $ go body
        return $ VPi binder' body'
      VLam binder closure -> do
        binder' <- traverse go binder
        closure' <- f binder closure
        return $ VLam binder' closure'

transformMixedClosureToStandardClosure ::
  forall m.
  (MonadLogic m, MonadBoundContext MixedLossValue m) =>
  MixedLossValue ->
  m (WHNFValue LossBuiltin)
transformMixedClosureToStandardClosure =
  traverseClosures eliminateStandardClosures
  where
    eliminateStandardClosures ::
      MixedLossBinder ->
      MixedClosure ->
      m (WHNFClosure LossBuiltin)
    eliminateStandardClosures binder = \case
      LossClos {} -> compilerDeveloperError "Impossible??"
      StandardClos (WHNFClosure env body) -> do
        boundCtx <- getBoundCtx (Proxy @MixedLossValue)
        let lv = boundCtxLv boundCtx
        let newEnv = extendEnvWithBound lv binder env
        addBinderToContext binder $ do
          mixedLossValue <- normStandardExprToLoss newEnv body
          lossValue <- transformMixedClosureToStandardClosure mixedLossValue
          let lossBody = quote mempty (lv + 1) lossValue
          let finalEnv = boundContextToEnv boundCtx
          return $ WHNFClosure finalEnv lossBody

transformLossClosureToMixedClosure ::
  (MonadCompile m) =>
  WHNFValue LossBuiltin ->
  m MixedLossValue
transformLossClosureToMixedClosure value =
  runFreshBoundContextT (Proxy @(WHNFValue LossBuiltin)) (traverseClosures eliminateLossClosure value)
  where
    eliminateLossClosure ::
      (MonadCompile m, MonadBoundContext (WHNFValue LossBuiltin) m) =>
      VBinder (WHNFClosure LossBuiltin) LossBuiltin ->
      WHNFClosure LossBuiltin ->
      m MixedClosure
    eliminateLossClosure _binder (WHNFClosure env body) = do
      newEnv <- traverse (\(b, v) -> (b,) <$> transformLossClosureToMixedClosure v) env
      return $ LossClos $ LossClosure newEnv body

--------------------------------------------------------------------------------
-- Conversion

instance EvaluableClosure MixedClosure LossBuiltin where
  formClosure env body = LossClos $ LossClosure env body

  evalClosure freeEnv closure (binder, arg) = case closure of
    StandardClos (WHNFClosure {}) ->
      developerError "Should not be evaluating standard closures"
    LossClos (LossClosure env body) -> do
      let newEnv = extendEnvWithDefined arg binder env
      eval freeEnv newEnv body

normStandardExprToLoss ::
  (MonadLogic m) =>
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m MixedLossValue
normStandardExprToLoss boundEnv expr = do
  standardValue <- normaliseInEnv boundEnv expr
  convertToLossBuiltins standardValue

convertToLossBuiltins ::
  forall m.
  (MonadLogic m) =>
  WHNFValue Builtin ->
  m MixedLossValue
convertToLossBuiltins e = do
  showEntry e
  result <- case e of
    VMeta {} ->
      unexpectedExprError currentPass "VMeta"
    VUniverse l ->
      return $ VUniverse l
    VFreeVar v spine -> do
      args' <- traverseArgs convertToLossBuiltins spine
      return $
        if v == identifierOf StdForeachIndex
          then VBuiltin ForeachIndex args'
          else VFreeVar v args'
    VBoundVar v spine -> do
      VBoundVar v <$> traverseArgs convertToLossBuiltins spine
    VBuiltin b spine -> do
      convertBuiltin b =<< traverseArgs convertToLossBuiltins spine
    VPi binder body -> do
      binder' <- traverse convertToLossBuiltins binder
      body' <- addBinderToContext binder' $ convertToLossBuiltins body
      return $ VPi binder' body'
    VLam binder closure -> do
      binder' <- traverse convertToLossBuiltins binder
      return $ VLam binder' (StandardClos closure)
  showExit result
  return result

convertBuiltin ::
  forall m.
  (MonadLogic m) =>
  Builtin ->
  MixedLossSpine ->
  m MixedLossValue
convertBuiltin builtin spine = convert builtin
  where
    convert :: Builtin -> m MixedLossValue
    convert b = case b of
      V.BuiltinType t -> convertBuiltinType t
      V.BuiltinConstructor c -> convertBuiltinConstructor c
      V.BuiltinFunction f -> convertBuiltinFunction f
      V.TypeClass {} -> unexpectedExprError currentPass "TypeClass"
      V.TypeClassOp {} -> unexpectedExprError currentPass "TypeClassOp"
      V.NatInDomainConstraint -> unexpectedExprError currentPass "NatInDomainConstraint"

    convertBuiltinType :: V.BuiltinType -> m MixedLossValue
    convertBuiltinType t = case t of
      V.Bool -> changedBuiltin L.Bool
      V.Unit -> unexpectedExprError currentPass "Unit"
      V.Nat -> unchangedBuiltin NatType
      V.Rat -> unchangedBuiltin RatType
      V.Index -> unchangedBuiltin IndexType
      V.List -> unchangedBuiltin ListType
      V.Vector -> unchangedBuiltin VectorType

    convertBuiltinConstructor :: V.BuiltinConstructor -> m MixedLossValue
    convertBuiltinConstructor c = case c of
      V.LUnit -> unexpectedExprError currentPass "LUnit"
      V.LBool True -> changedBuiltin L.Truthity
      V.LBool False -> changedBuiltin L.Falsity
      V.Nil -> unchangedBuiltin NilList
      V.Cons -> unchangedBuiltin ConsList
      V.LIndex x -> unchangedBuiltin (Index x)
      V.LNat x -> unchangedBuiltin (Nat x)
      V.LRat x -> unchangedBuiltin (Rat x)
      V.LVec n -> unchangedBuiltin $ Vector n

    convertBuiltinFunction :: V.BuiltinFunction -> m MixedLossValue
    convertBuiltinFunction b = case b of
      V.FromNat {} -> unexpectedExprError currentPass "FromNat"
      V.FromRat {} -> unexpectedExprError currentPass "FromRat"
      ------------------------
      -- Boolean operations --
      ------------------------
      V.Not -> changedBuiltin L.Negation
      V.And -> changedBuiltin L.Conjunction
      V.Or -> changedBuiltin L.Disjunction
      V.Implies -> changedBuiltin L.Implication
      -- Rational comparisons
      V.Equals V.EqRat V.Eq -> changedBuiltin L.Equal
      V.Equals V.EqRat V.Neq -> changedBuiltin L.NotEqual
      V.Order V.OrderRat V.Lt -> changedBuiltin L.LessThan
      V.Order V.OrderRat V.Le -> changedBuiltin L.LessEqual
      V.Order V.OrderRat V.Gt -> changedBuiltin L.GreaterThan
      V.Order V.OrderRat V.Ge -> changedBuiltin L.GreaterEqual
      -- Quantifiers
      V.Quantifier V.Exists -> translateExists spine
      V.Quantifier V.Forall -> translateForall spine
      -- Unsupported
      V.If -> do
        declProv <- getDeclProvenance
        throwError $ UnsupportedIfOperation declProv mempty
      V.Equals V.EqIndex V.Eq -> unsupportedTypeError b
      V.Equals V.EqIndex V.Neq -> unsupportedTypeError b
      V.Order V.OrderIndex V.Le -> unsupportedTypeError b
      V.Order V.OrderIndex V.Lt -> unsupportedTypeError b
      V.Order V.OrderIndex V.Ge -> unsupportedTypeError b
      V.Order V.OrderIndex V.Gt -> unsupportedTypeError b
      V.Equals V.EqNat V.Eq -> unsupportedTypeError b
      V.Equals V.EqNat V.Neq -> unsupportedTypeError b
      V.Order V.OrderNat V.Le -> unsupportedTypeError b
      V.Order V.OrderNat V.Lt -> unsupportedTypeError b
      V.Order V.OrderNat V.Ge -> unsupportedTypeError b
      V.Order V.OrderNat V.Gt -> unsupportedTypeError b
      ----------------------
      -- Other operations --
      ----------------------
      V.Neg dom -> unchangedBuiltin (Neg dom)
      V.Add dom -> unchangedBuiltin (Add dom)
      V.Sub dom -> unchangedBuiltin (Sub dom)
      V.Mul dom -> unchangedBuiltin (Mul dom)
      V.Div dom -> unchangedBuiltin (Div dom)
      V.PowRat -> unchangedBuiltin PowRat
      V.MinRat -> unchangedBuiltin MinRat
      V.MaxRat -> unchangedBuiltin MaxRat
      V.FoldVector -> unchangedBuiltin FoldVector
      V.MapVector -> unchangedBuiltin MapVector
      V.ZipWithVector -> unchangedBuiltin ZipWithVector
      V.Indices -> unchangedBuiltin Indices
      V.At -> unchangedBuiltin LookupVector
      V.FoldList -> unchangedBuiltin FoldList
      V.MapList -> unchangedBuiltin MapList
      where
        unsupportedTypeError t = compilerDeveloperError $ "Conversion of" <+> pretty t <+> "not yet supported"

    changedBuiltin :: DifferentiableLogicField -> m MixedLossValue
    changedBuiltin field = do
      fn <- lookupLogicField field
      logDebug MaxDetail $ "subst-field" <+> pretty field <+> "-->" <+> prettyVerbose fn
      evalApp mempty fn spine

    unchangedBuiltin :: LossBuiltin -> m MixedLossValue
    unchangedBuiltin b = return $ VBuiltin b spine

convertNot :: (MonadLogic m) => MixedLossSpine -> m MixedLossValue
convertNot = convertBuiltin (V.BuiltinFunction V.Not)

translateExists :: (MonadLogic m) => MixedLossSpine -> m MixedLossValue
translateExists spine = do
  disjunct <- lookupLogicField Disjunction
  return $ VBuiltin Search (explicit disjunct : spine)

translateForall :: (MonadLogic m) => MixedLossSpine -> m MixedLossValue
translateForall spine = case spine of
  [argExpr -> VLam binder closure] -> do
    negBody <- case closure of
      StandardClos (WHNFClosure env body) -> return $ StandardClos $ WHNFClosure env (INot body)
      LossClos {} -> developerError "Should not be evaluating standard closures"
    let newSpine = [explicit (VLam binder negBody)]
    existsExpr <- translateExists newSpine
    convertNot [explicit existsExpr]
  _ -> unexpectedExprError currentPass (pretty V.Forall <+> prettyVerbose spine)

--------------------------------------------------------------------------------
-- Utils

currentPass :: CompilerPass
currentPass = "logic translation"

showEntry :: (MonadLogic m) => WHNFValue Builtin -> m ()
showEntry e = do
  ctx <- getNamedBoundCtx (Proxy @MixedLossValue)
  logDebug MaxDetail $ "loss-enter:" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadLogic m) => MixedLossValue -> m ()
showExit e = do
  ctx <- getNamedBoundCtx (Proxy @MixedLossValue)
  decrCallDepth
  logDebug MaxDetail $ "loss-exit: " <+> prettyFriendly (WithContext e ctx)

-- [max 0 (0 - f [0, 0] ! 0), max 0 (0 - f [0, 0] ! 1)]
-- max [0,0] ([0,0] - f [0, 0])
