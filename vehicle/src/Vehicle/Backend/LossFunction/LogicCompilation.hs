{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.LogicCompilation
  ( compileLogic,
    convertToLossBuiltins,
    normStandardExprToLoss,
    normLossExprToLoss,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Backend.LossFunction.Core
import Vehicle.Backend.LossFunction.Logics qualified as DSL
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (EvaluableClosure (..), eval, evalApp)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard (Quantifier)
import Vehicle.Data.DSL (fromDSL)
import Vehicle.Data.Expr.Normalised (Value (..), WHNFBoundEnv, WHNFValue, extendEnvWithDefined)
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))
import Vehicle.Syntax.Builtin (Builtin)
import Vehicle.Syntax.Builtin qualified as V

--------------------------------------------------------------------------------
-- Logic setup

-- | Compiles a differentiable logic from the DSL for expressing them into a
-- form suitable for normalisation. Eventually the DSL should be replaced by
-- the something in the language.
compileLogic :: (MonadCompile m) => DSL.DifferentialLogicDSL -> m DifferentialLogicImplementation
compileLogic dsl = do
  let logicID = DSL.logicID dsl
  let translate t = eval mempty mempty $ fromDSL mempty (t dsl)

  -- Translate the different expressions
  translateBool <- translate DSL.translateBool
  translateTrue <- translate DSL.translateTrue
  translateFalse <- translate DSL.translateFalse
  translateAnd <- translate DSL.translateAnd
  translateOr <- translate DSL.translateOr
  translateNot <- translate DSL.translateNot
  translateImplies <- translate DSL.translateImplies
  translateLe <- translate DSL.translateLe
  translateLt <- translate DSL.translateLt
  translateGe <- translate DSL.translateGe
  translateGt <- translate DSL.translateGt
  translateNeq <- translate DSL.translateNeq
  translateEq <- translate DSL.translateEq

  -- Translate the different expressions
  return $ DifferentialLogicImplementation {..}

--------------------------------------------------------------------------------
-- Conversion

instance EvaluableClosure MixedClosure LossBuiltin where
  formClosure = LossClosure

  evalClosure freeEnv closure (binder, arg) = case closure of
    StandardClosure {} ->
      compilerDeveloperError "Not possible???"
    LossClosure boundEnv body -> do
      let newEnv = extendEnvWithDefined arg binder boundEnv
      eval freeEnv newEnv body

normStandardExprToLoss ::
  (MonadLoss m) =>
  WHNFBoundEnv Builtin ->
  Expr Ix Builtin ->
  m MixedLossValue
normStandardExprToLoss boundEnv expr = do
  freeEnv <- getStandardFreeEnvWithoutHidden
  standardValue <- eval freeEnv boundEnv expr
  convertToLossBuiltins standardValue

normLossExprToLoss ::
  (MonadLoss m) =>
  MixedBoundEnv ->
  Expr Ix LossBuiltin ->
  m MixedLossValue
normLossExprToLoss boundEnv expr = do
  freeEnv <- getLossFreeEnvWithoutHidden
  eval freeEnv boundEnv expr

convertToLossBuiltins ::
  forall m.
  (MonadLoss m) =>
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
      return $ VLam binder' (StandardClosure closure)
  showExit result
  return result

convertBuiltin ::
  forall m.
  (MonadLoss m) =>
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
      V.Bool -> changedBuiltin translateBool
      V.Unit -> unexpectedExprError currentPass "Unit"
      V.Nat -> unchangedBuiltin NatType
      V.Rat -> unchangedBuiltin RatType
      V.Index -> unchangedBuiltin IndexType
      V.List -> unchangedBuiltin ListType
      V.Vector -> unchangedBuiltin VectorType

    convertBuiltinConstructor :: V.BuiltinConstructor -> m MixedLossValue
    convertBuiltinConstructor c = case c of
      V.LUnit -> unexpectedExprError currentPass "LUnit"
      V.LBool True -> changedBuiltin translateTrue
      V.LBool False -> changedBuiltin translateFalse
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
      V.Not -> changedBuiltin translateNot
      V.And -> changedBuiltin translateAnd
      V.Or -> changedBuiltin translateOr
      V.Implies -> changedBuiltin translateImplies
      -- Rational comparisons
      V.Equals V.EqRat V.Eq -> changedBuiltin translateEq
      V.Equals V.EqRat V.Neq -> changedBuiltin translateNeq
      V.Order V.OrderRat V.Lt -> changedBuiltin translateLt
      V.Order V.OrderRat V.Le -> changedBuiltin translateLe
      V.Order V.OrderRat V.Gt -> changedBuiltin translateGt
      V.Order V.OrderRat V.Ge -> changedBuiltin translateGe
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

    changedBuiltin :: (DifferentialLogicImplementation -> MixedLossValue) -> m MixedLossValue
    changedBuiltin translation = do
      logic <- getLogic
      freeEnv <- getLossFreeEnvWithoutHidden
      evalApp freeEnv (translation logic) spine

    unchangedBuiltin :: LossBuiltin -> m MixedLossValue
    unchangedBuiltin b = return $ VBuiltin b spine

translateQuantifier :: (MonadLoss m) => Quantifier -> MixedLossSpine -> m MixedLossValue
translateQuantifier q spine = do
  let b = case q of
        V.Forall -> Minimise
        V.Exists -> Maximise
  return $ VBuiltin b spine

--------------------------------------------------------------------------------
-- Utils

currentPass :: CompilerPass
currentPass = "logic translation"

showEntry :: (MonadLoss m) => WHNFValue Builtin -> m ()
showEntry e = do
  ctx <- getNamedBoundCtx
  logDebug MaxDetail $ "loss-enter:" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadLoss m) => MixedLossValue -> m ()
showExit e = do
  ctx <- getNamedBoundCtx
  decrCallDepth
  logDebug MaxDetail $ "loss-exit: " <+> prettyFriendly (WithContext e ctx)
