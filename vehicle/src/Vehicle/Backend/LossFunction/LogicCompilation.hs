{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.LogicCompilation
  ( compileLogic,
    convertToLossBuiltins,
    normStandardExprToLoss,
    normLossExprToLoss,
    MonadLogicCtx,
    runMonadLogicT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.Map qualified as Map
import Vehicle.Backend.LossFunction.Core
import Vehicle.Backend.LossFunction.Core qualified as L
import Vehicle.Backend.LossFunction.Logics qualified as DSL
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (EvaluableClosure (..), eval, evalApp)
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard (Quantifier)
import Vehicle.Data.Expr.Normalised (VBinder, Value (..), WHNFBoundEnv, WHNFClosure (..), WHNFValue, boundContextToEnv, extendEnvWithBound, extendEnvWithDefined)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Syntax.Builtin (Builtin)
import Vehicle.Syntax.Builtin qualified as V

--------------------------------------------------------------------------------
-- Monad

type MonadLogicCtx =
  ( DifferentiableLogicID,
    Either DeclProvenance DifferentiableLogicField,
    DifferentiableLogicImplementation,
    FreeEnv (WHNFClosure Builtin) Builtin,
    GenericBoundCtx MixedLossBinder
  )

type MonadLogic m =
  ( MonadCompile m,
    MonadReader MonadLogicCtx m
  )

runMonadLogicT ::
  (MonadCompile m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  Either DeclProvenance DifferentiableLogicField ->
  FreeEnv (WHNFClosure Builtin) Builtin ->
  GenericBoundCtx MixedLossBinder ->
  ReaderT MonadLogicCtx m a ->
  m a
runMonadLogicT logicID logic origin standardEnv boundCtx =
  flip runReaderT (logicID, origin, logic, standardEnv, boundCtx)

getLogic :: (MonadLogic m) => m DifferentiableLogicImplementation
getLogic = do
  (_, _, logic, _, _) <- ask
  return logic

getDeclProvenance :: (MonadLogic m) => m (Either DeclProvenance DifferentiableLogicField)
getDeclProvenance = do
  (_, prov, _, _, _) <- ask
  return prov

getStandardFreeEnvWithoutHidden :: (MonadLogic m) => m (FreeEnv (WHNFClosure Builtin) Builtin)
getStandardFreeEnvWithoutHidden = do
  (_, _, _, env, _) <- ask
  return $ Map.map (\d -> if isPreservedStdLibOp d then convertToPostulate d else d) env

getBoundCtx :: (MonadLogic m) => m (GenericBoundCtx MixedLossBinder)
getBoundCtx = do
  (_, _, _, _, ctx) <- ask
  return ctx

getNamedBoundCtx :: (MonadLogic m) => m NamedBoundCtx
getNamedBoundCtx = do
  (_, _, _, _, ctx) <- ask
  return $ fmap nameOf ctx

addLossBinderToContext :: (MonadLogic m) => MixedLossBinder -> m a -> m a
addLossBinderToContext binder cont = do
  local
    ( \(logicID, declProv, logic, standardEnv, ctx) ->
        (logicID, declProv, logic, standardEnv, binder : ctx)
    )
    cont

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
          runMonadLogicT logicID impl (Right field) mempty mempty $ do
            mixedLossValue <- normStandardExprToLoss mempty expr
            lossValue <- transformMixedClosureToStandardClosure mixedLossValue
            transformLossClosureToMixedClosure lossValue
      go (Map.insert field value impl) fields

traverseClosures ::
  forall m closure1 closure2 builtin.
  (Monad m) =>
  (forall a. VBinder closure1 builtin -> m a -> m a) ->
  (VBinder closure1 builtin -> closure1 -> m closure2) ->
  Value closure1 builtin ->
  m (Value closure2 builtin)
traverseClosures addBinderToCtx f = go
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
        body' <- addBinderToCtx binder $ go body
        return $ VPi binder' body'
      VLam binder closure -> do
        binder' <- traverse go binder
        closure' <- addBinderToCtx binder $ f binder closure
        return $ VLam binder' closure'

transformMixedClosureToStandardClosure ::
  forall m.
  (MonadLogic m) =>
  MixedLossValue ->
  m (WHNFValue LossBuiltin)
transformMixedClosureToStandardClosure =
  traverseClosures addLossBinderToContext eliminateStandardClosures
  where
    eliminateStandardClosures ::
      MixedLossBinder ->
      MixedClosure ->
      m (WHNFClosure LossBuiltin)
    eliminateStandardClosures binder = \case
      LossClos {} -> compilerDeveloperError "Impossible??"
      StandardClos (WHNFClosure env body) -> do
        boundCtx <- getBoundCtx
        let lv = boundCtxLv boundCtx
        let newEnv = extendEnvWithBound (lv - 1) binder env
        mixedLossValue <- normStandardExprToLoss newEnv body
        lossValue <- transformMixedClosureToStandardClosure mixedLossValue
        let lossBody = quote mempty lv lossValue
        let finalEnv = boundContextToEnv (tail boundCtx)
        return $ WHNFClosure finalEnv lossBody

transformLossClosureToMixedClosure ::
  forall m.
  (MonadLogic m) =>
  WHNFValue LossBuiltin ->
  m MixedLossValue
transformLossClosureToMixedClosure =
  traverseClosures (\_ x -> x) eliminateLossClosure
  where
    eliminateLossClosure ::
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
      compilerDeveloperError "Should not be evaluating standard closures"
    LossClos (LossClosure env body) -> do
      let newEnv = extendEnvWithDefined arg binder env
      eval freeEnv newEnv body

normStandardExprToLoss ::
  (MonadLogic m) =>
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m MixedLossValue
normStandardExprToLoss boundEnv expr = do
  freeEnv <- getStandardFreeEnvWithoutHidden
  standardValue <- eval freeEnv boundEnv expr
  result <- convertToLossBuiltins standardValue
  return result

normLossExprToLoss ::
  (MonadLogic m) =>
  MixedBoundEnv ->
  Expr Ix LossBuiltin ->
  m MixedLossValue
normLossExprToLoss = eval mempty

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
      body' <- addLossBinderToContext binder' $ convertToLossBuiltins body
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
      V.LVec _x -> unchangedBuiltin Vector

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
      V.Quantifier q -> translateQuantifier q spine
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

translateQuantifier :: (MonadLogic m) => Quantifier -> MixedLossSpine -> m MixedLossValue
translateQuantifier q spine = do
  (builtin, op) <- case q of
    V.Forall -> (Minimise,) <$> lookupLogicField Conjunction
    V.Exists -> (Maximise,) <$> lookupLogicField Disjunction
  return $ VBuiltin builtin (explicit op : spine)

--------------------------------------------------------------------------------
-- Utils

currentPass :: CompilerPass
currentPass = "logic translation"

showEntry :: (MonadLogic m) => WHNFValue Builtin -> m ()
showEntry e = do
  ctx <- getNamedBoundCtx
  logDebug MaxDetail $ "loss-enter:" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadLogic m) => MixedLossValue -> m ()
showExit e = do
  ctx <- getNamedBoundCtx
  decrCallDepth
  logDebug MaxDetail $ "loss-exit: " <+> prettyFriendly (WithContext e ctx)

-- [max 0 (0 - f [0, 0] ! 0), max 0 (0 - f [0, 0] ! 1)]
-- max [0,0] ([0,0] - f [0, 0])
