{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.Builtin
  ( evalBuiltin,
    evalMulRat,
    evalAddRat,
    evalSubRat,
    evalDivRat,
    evalNegRat,
    evalEqualityRat,
    evalEquals,
    evalOrderRat,
    evalOrder,
    evalAt,
    evalFoldVector,
    evalMapVector,
    evalZipWith,
    evalIndices,
  )
where

import Control.Monad (zipWithM)
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe, mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Builtin

-- Okay so the important thing to remember about this module is that we have
-- a variety of different typing schemes for builtins (standard, polarity,
-- linearity etc.). Normalisation needs to work for all of these, and
-- therefore we can't guarantee what the implicit and instance arguments are
-- going to be for a given builtin. However, explicit arguments are always
-- the same in every type system.

-- Therefore this can be viewed as a type of runtime irrelevance, where only
-- the explicit arguments are runtime relevant. This notion isn't made
-- explicit in the code below. Maybe there's a nice way of doing so?

-----------------------------------------------------------------------------
-- Main method

evalBuiltin ::
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  EvalApp builtin m ->
  builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)
evalBuiltin evalApp b args = do
  let runtimeRelevantArgs = filterRuntimeRelevantArgs args
  case tryToEvaluateOnRuntimeRelevantArgs evalApp b runtimeRelevantArgs of
    Just result -> result
    _ -> return $ VBuiltin b args

-----------------------------------------------------------------------------
-- Runtime relevance
{-
isRuntimeRelevant :: WHNFArg builtin -> Bool
isRuntimeRelevant = isExplicit
-}
filterRuntimeRelevantArgs :: WHNFSpine builtin -> [WHNFValue builtin]
filterRuntimeRelevantArgs = mapMaybe getExplicitArg

-----------------------------------------------------------------------------
-- Builtin evaluation

-- | A method for evaluating an application.
-- Although there is only one implementation of this type, it needs to be
-- passed around as an argument to avoid dependency cycles between
-- this module and the module in which the general NBE algorithm lives in.
type EvalApp builtin m =
  WHNFValue builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)

-- | A method for evaluating builtins that takes in an argument allowing the
-- recursive evaluation of applications. that takes in an argument allowing
-- the subsequent further evaluation of applications.
-- Such recursive evaluation is necessary when evaluating higher order
-- functions such as fold, map etc.
type EvalBuiltin builtin m =
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  [WHNFValue builtin] ->
  Maybe (m (WHNFValue builtin))

type EvalSimpleBuiltin builtin =
  (HasStandardData builtin) =>
  [WHNFValue builtin] ->
  Maybe (WHNFValue builtin)

tryToEvaluateOnRuntimeRelevantArgs ::
  (MonadCompile m, HasStandardData builtin, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  builtin ->
  [WHNFValue builtin] ->
  Maybe (m (WHNFValue builtin))
tryToEvaluateOnRuntimeRelevantArgs evalApp b runtimeRelevantArgs =
  case getBuiltinFunction b of
    Just f -> evalBuiltinFunction evalApp f runtimeRelevantArgs
    Nothing -> Nothing

evalBuiltinFunction ::
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  EvalApp builtin m ->
  BuiltinFunction ->
  [WHNFValue builtin] ->
  Maybe (m (WHNFValue builtin))
evalBuiltinFunction evalApp b args = case b of
  Quantifier {} -> Nothing
  Optimise {} -> Nothing
  Not -> return <$> evalNot args
  And -> return <$> evalAnd args
  Or -> return <$> evalOr args
  Neg dom -> return <$> evalNeg dom args
  Add dom -> return <$> evalAdd dom args
  Sub dom -> return <$> evalSub dom args
  Mul dom -> return <$> evalMul dom args
  Div dom -> return <$> evalDiv dom args
  PowRat -> return <$> evalPowRat args
  MinRat -> return <$> evalMinRat args
  MaxRat -> return <$> evalMaxRat args
  Equals dom op -> return <$> evalEquals dom op args
  Order dom op -> return <$> evalOrder dom op args
  If -> return <$> evalIf args
  At -> return <$> evalAt args
  Fold dom -> evalFold dom evalApp args
  ZipWithVector -> evalZipWith evalApp args
  MapList -> evalMapList evalApp args
  MapVector -> evalMapVector evalApp args
  FromNat dom -> return <$> evalFromNat dom args
  FromRat dom -> return <$> evalFromRat dom args
  Indices -> return <$> evalIndices args
  Implies -> return <$> evalImplies args

-----------------------------------------------------------------------------
-- Blocking
-----------------------------------------------------------------------------
{-
-- | Indices into the the list of runtime-relevant arguments, indicating which
-- arguments are blocking the evaluation of the builtin.
-- Numbering starts from 0 at the front the list.
-- NOTE: a difference list would better preserve the invariant that this should
-- always be monotonically ascending.
type BlockingArgs = [Int]

traverseBuiltinBlockingArgs ::
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  (WHNFValue builtin -> m (WHNFValue builtin)) ->
  builtin ->
  WHNFSpine builtin ->
  m (WHNFSpine builtin)
traverseBuiltinBlockingArgs f b args = case getBuiltinFunction b of
  Just func -> traverseBuiltinFunctionBlockingArgs f func args
  Nothing -> return args

traverseBuiltinFunctionBlockingArgs ::
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  (WHNFValue builtin -> m (WHNFValue builtin)) ->
  BuiltinFunction ->
  WHNFSpine builtin ->
  m (WHNFSpine builtin)
traverseBuiltinFunctionBlockingArgs f b args =
  traverseBlockingArgs f b args $ case b of
    Quantifier {} -> []
    Optimise {} -> []
    Not -> [0]
    And -> [0, 1]
    Or -> [0, 1]
    Neg {} -> [0]
    Add {} -> [0, 1]
    Sub {} -> [0, 1]
    Mul {} -> [0, 1]
    Div {} -> [0, 1]
    PowRat -> [0, 1]
    MinRat -> [0, 1]
    MaxRat -> [0, 1]
    Equals {} -> [0, 1]
    Order {} -> [0, 1]
    If -> [0]
    At -> [0, 1]
    Fold {} -> [2]
    ZipWithVector -> [1, 2]
    MapList -> [1]
    MapVector -> [1]
    FromNat {} -> [0]
    FromRat {} -> [0]
    Indices -> [0]
    Implies -> [0, 1]
    Ann -> []

traverseBlockingArgs ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  (WHNFValue builtin -> m (WHNFValue builtin)) ->
  BuiltinFunction ->
  WHNFSpine builtin ->
  BlockingArgs ->
  m (WHNFSpine builtin)
traverseBlockingArgs f b originalSpine originalBlockingArgs = go 0 originalSpine originalBlockingArgs
  where
    go ::
      Int ->
      WHNFSpine builtin ->
      BlockingArgs ->
      m (WHNFSpine builtin)
    go _ spine [] = return spine
    go _ [] _ =
      compilerDeveloperError $
        "run out of args when traversing blocked arguments"
          <+> pretty originalBlockingArgs
          <+> "in spine"
          <+> prettyVerbose originalSpine
          <+> "for"
          <+> quotePretty b
    go argNo (arg : args) (blockedArg : blockedArgs)
      | isRuntimeRelevant arg && argNo == blockedArg = (:) <$> traverse f arg <*> go (argNo + 1) args blockedArgs
      | isRuntimeRelevant arg = (arg :) <$> go (argNo + 1) args (blockedArg : blockedArgs)
      | otherwise = (arg :) <$> go argNo args (blockedArg : blockedArgs)
-}
-----------------------------------------------------------------------------
-- Individual builtin evaluation
-----------------------------------------------------------------------------

evalNot :: EvalSimpleBuiltin builtin
evalNot e = case e of
  [VBoolLiteral x] -> Just $ VBoolLiteral (not x)
  _ -> Nothing

evalAnd :: EvalSimpleBuiltin builtin
evalAnd = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ VBoolLiteral (x && y)
  [VBoolLiteral b, y] -> Just $ if b then y else VBoolLiteral b
  [x, VBoolLiteral b] -> Just $ if b then x else VBoolLiteral b
  _ -> Nothing

evalOr :: EvalSimpleBuiltin builtin
evalOr = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ VBoolLiteral (x || y)
  [VBoolLiteral b, y] -> Just $ if b then VBoolLiteral b else y
  [x, VBoolLiteral b] -> Just $ if b then VBoolLiteral b else x
  _ -> Nothing

evalNeg :: NegDomain -> EvalSimpleBuiltin builtin
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: EvalSimpleBuiltin builtin
evalNegInt = \case
  [VIntLiteral x] -> Just $ VIntLiteral (-x)
  _ -> Nothing

evalNegRat :: EvalSimpleBuiltin builtin
evalNegRat = \case
  [VRatLiteral x] -> Just $ VRatLiteral (-x)
  _ -> Nothing

evalAdd :: AddDomain -> EvalSimpleBuiltin builtin
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: EvalSimpleBuiltin builtin
evalAddNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VNatLiteral (x + y)
  _ -> Nothing

evalAddInt :: EvalSimpleBuiltin builtin
evalAddInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x + y)
  _ -> Nothing

evalAddRat :: EvalSimpleBuiltin builtin
evalAddRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x + y)
  _ -> Nothing

evalSub :: SubDomain -> EvalSimpleBuiltin builtin
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: EvalSimpleBuiltin builtin
evalSubInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x - y)
  _ -> Nothing

evalSubRat :: EvalSimpleBuiltin builtin
evalSubRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x - y)
  _ -> Nothing

evalMul :: MulDomain -> EvalSimpleBuiltin builtin
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: EvalSimpleBuiltin builtin
evalMulNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VNatLiteral (x * y)
  _ -> Nothing

evalMulInt :: EvalSimpleBuiltin builtin
evalMulInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x * y)
  _ -> Nothing

evalMulRat :: EvalSimpleBuiltin builtin
evalMulRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x * y)
  _ -> Nothing

evalDiv :: DivDomain -> EvalSimpleBuiltin builtin
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: EvalSimpleBuiltin builtin
evalDivRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x / y)
  _ -> Nothing

evalPowRat :: EvalSimpleBuiltin builtin
evalPowRat = \case
  [VRatLiteral x, VIntLiteral y] -> Just $ VRatLiteral (x ^^ y)
  _ -> Nothing

evalMinRat :: EvalSimpleBuiltin builtin
evalMinRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (min x y)
  _ -> Nothing

evalMaxRat :: EvalSimpleBuiltin builtin
evalMaxRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (max x y)
  _ -> Nothing

evalOrder :: OrderDomain -> OrderOp -> EvalSimpleBuiltin builtin
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderNat :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderInt :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderRat :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalEquals :: EqualityDomain -> EqualityOp -> EvalSimpleBuiltin builtin
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityNat :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityInt :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityRat :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalFromNat :: FromNatDomain -> EvalSimpleBuiltin builtin
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: EvalSimpleBuiltin builtin
evalFromNatToIndex = \case
  [VNatLiteral x] -> Just $ VIndexLiteral x
  _ -> Nothing

evalFromNatToNat :: EvalSimpleBuiltin builtin
evalFromNatToNat = \case
  [x] -> Just x
  _ -> Nothing

evalFromNatToInt :: EvalSimpleBuiltin builtin
evalFromNatToInt = \case
  [VNatLiteral x] -> Just $ VIntLiteral x
  _ -> Nothing

evalFromNatToRat :: EvalSimpleBuiltin builtin
evalFromNatToRat = \case
  [VNatLiteral x] -> Just $ VRatLiteral (fromIntegral x)
  _ -> Nothing

evalFromRat :: FromRatDomain -> EvalSimpleBuiltin builtin
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: EvalSimpleBuiltin builtin
evalFromRatToRat = \case
  [x] -> Just x
  _ -> Nothing

evalIf :: EvalSimpleBuiltin builtin
evalIf = \case
  [VBoolLiteral True, e1, _e2] -> Just e1
  [VBoolLiteral False, _e1, e2] -> Just e2
  _ -> Nothing

evalAt :: EvalSimpleBuiltin builtin
evalAt = \case
  [VVecLiteral xs, VIndexLiteral i] -> Just $ case xs !!? fromIntegral i of
    Nothing -> developerError $ "out of bounds error:" <+> pretty (length xs) <+> "<=" <+> pretty i
    Just xsi -> argExpr xsi
  _ -> Nothing

evalFold ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  FoldDomain ->
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalFold = \case
  FoldList -> evalFoldList
  FoldVector -> evalFoldVector

evalFoldList ::
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalFoldList evalApp = \case
  [_f, e, VNil] ->
    Just $ return e
  [f, e, VCons x xs] ->
    Just $ do
      let defaultFold = return $ VBuiltin (mkBuiltinFunction (Fold FoldList)) [Arg mempty Explicit Relevant f, Arg mempty Explicit Relevant e, xs]
      r <- fromMaybe defaultFold $ evalFoldList evalApp [f, e, argExpr xs]
      evalApp f [x, Arg mempty Explicit Relevant r]
  _ -> Nothing

evalFoldVector ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalFoldVector evalApp = \case
  [f, e, VVecLiteral xs] ->
    Just $ foldrM f' e xs
    where
      f' x r =
        evalApp
          f
          [ x,
            Arg mempty Explicit Relevant r
          ]
  _ -> Nothing

evalZipWith ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalZipWith evalApp = \case
  [f, VVecLiteral xs, VVecLiteral ys] ->
    Just $ mkVLVec <$> zipWithM f' xs ys
    where
      f' x y =
        evalApp
          f
          [ x,
            y
          ]
  _ -> Nothing

evalMapList ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalMapList evalApp = \case
  [_f, e@VNil] ->
    Just $ return e
  [f, VCons x xs] -> Just $ do
    fx <- evalApp f [x]
    fxs <- case evalMapList evalApp [f, argExpr xs] of
      Nothing -> return $ VBuiltin (mkBuiltinFunction MapList) [Arg mempty Explicit Relevant f, xs]
      Just fxs -> fxs
    return $ VBuiltin (mkBuiltinConstructor Cons) (Arg mempty Explicit Relevant <$> [fx, fxs])
  _ -> Nothing

evalMapVector ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalMapVector evalApp = \case
  [f, VVecLiteral xs] ->
    Just $ mkVLVec <$> traverse f' xs
    where
      f' x = evalApp f [x]
  _ -> Nothing

evalIndices :: EvalSimpleBuiltin builtin
evalIndices = \case
  [VNatLiteral n] -> Just $ mkVLVec (fmap VIndexLiteral [0 .. n - 1])
  _ -> Nothing

-----------------------------------------------------------------------------
-- Derived

-- TODO define in terms of language. The problem is the polarity checking...
evalImplies :: EvalSimpleBuiltin builtin
evalImplies = \case
  [e1, e2] -> Just $ do
    let defaultNot = VBuiltin (mkBuiltinFunction Not) [Arg mempty Explicit Relevant e1]
    let ne1 = fromMaybe defaultNot (evalNot [e1])
    let defaultOr = VBuiltin (mkBuiltinFunction Or) [Arg mempty Explicit Relevant ne1, Arg mempty Explicit Relevant e2]
    let maybeRes = evalOr [ne1, e2]
    fromMaybe defaultOr maybeRes
  _ -> Nothing
