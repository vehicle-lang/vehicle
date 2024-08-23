{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.Builtin where

import Control.Monad (zipWithM)
import Data.Foldable (foldrM)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Builtin
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface (BuiltinHasStandardData (..))
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Polarity (PolarityBuiltin (..))
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor, foldTensor, mapTensor, zipWithTensor)

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

-- | Type signature for the most generic method of evaluating a builtin
-- application. Crucially it takes both the method of evaluating an expression
-- and the unnormalised arguments, because it needs to be able to support
-- the non-compositional normalisation, e.g. translation to Loss functions
-- as the DL2 translation of `not` isn't compositional and therefore we
-- need access the unnormalised form of the arguments.
--
-- This can't be a type-class because it's not type directed. Different
-- Differential logics all normalise to the same type but have different
-- methods of normalisation.
type EvalBuiltinApp m builtin =
  (MonadCompile m) =>
  EvalApp (WHNFValue builtin) m ->
  Eval builtin m ->
  builtin ->
  [Arg builtin] ->
  m (WHNFValue builtin)

type Eval builtin m =
  (MonadCompile m) =>
  Expr builtin ->
  m (WHNFValue builtin)

evalTypeClassOp ::
  (MonadLogger m, BuiltinHasStandardData builtin, Show builtin) =>
  (BuiltinFunction -> EvalBuiltin (Value closure builtin) m) ->
  EvalApp (Value closure builtin) m ->
  Value closure builtin ->
  builtin ->
  Spine closure builtin ->
  m (Value closure builtin)
evalTypeClassOp evalFn evalApp originalExpr b normArgs
  | isTypeClassOp b = do
      (inst, remainingArgs) <- findInstanceArg b normArgs
      evalApp inst remainingArgs
  | otherwise = case getBuiltinFunction b of
      Nothing -> return originalExpr
      Just f -> evalFn f evalApp originalExpr normArgs

findInstanceArg :: (MonadLogger m, Show op) => op -> [GenericArg a] -> m (a, [GenericArg a])
findInstanceArg op = \case
  (InstanceArg _ _ inst : xs) -> return (inst, xs)
  (_ : xs) -> findInstanceArg op xs
  [] -> developerError $ "Malformed type class operation:" <+> pretty (show op)

filterOutIrrelevantArgs :: [GenericArg expr] -> [GenericArg expr]
filterOutIrrelevantArgs = filter isRelevant

-----------------------------------------------------------------------------
-- Builtin evaluation

-- | A method for evaluating an application.
-- Although there is only one implementation of this type, it needs to be
-- passed around as an argument to avoid dependency cycles between
-- this module and the module in which the general NBE algorithm lives in.
type EvalApp expr m = expr -> [GenericArg expr] -> m expr

--------------------------------------------------------------------------------
-- Evaluation

-- | A method for evaluating builtins that takes in an argument allowing the
-- recursive evaluation of applications. that takes in an argument allowing
-- the subsequent further evaluation of applications.
-- Such recursive evaluation is necessary when evaluating higher order
-- functions such as fold, map etc.
type EvalBuiltin expr m =
  (MonadLogger m) =>
  EvalApp expr m ->
  expr ->
  [GenericArg expr] ->
  m expr

type EvalSimpleBuiltin expr =
  expr ->
  [GenericArg expr] ->
  expr

evalOp1 ::
  (expr -> Maybe (Provenance, a)) ->
  (a -> b) ->
  (Provenance -> b -> expr) ->
  EvalSimpleBuiltin expr
evalOp1 getArg op mkResult originalExpr = \case
  [argExpr -> (getArg -> Just (p, x))] -> mkResult p (op x)
  _ -> originalExpr

evalOp2 ::
  (expr -> Maybe (Provenance, a)) ->
  (expr -> Maybe (Provenance, b)) ->
  (a -> b -> c) ->
  (Provenance -> c -> expr) ->
  EvalSimpleBuiltin expr
evalOp2 getArg1 getArg2 op mkResult originalExpr = \case
  [argExpr -> (getArg1 -> Just (p, x)), argExpr -> (getArg2 -> Just (_, y))] -> mkResult p (op x y)
  _ -> originalExpr

evalReduceTensor ::
  (expr -> Maybe (Provenance, Tensor a)) ->
  (Tensor a -> Tensor a -> Tensor a) ->
  (Provenance -> Tensor a -> expr) ->
  EvalSimpleBuiltin expr
evalReduceTensor getTensor f mkTensor originalExpr = \case
  [argExpr -> (getTensor -> Just (_, e)), argExpr -> (getTensor -> Just (_, t))] ->
    mkTensor mempty $ foldTensor f e t
  _ -> originalExpr

-----------------------------------------------------------------------------
-- Individual builtin evaluation
-----------------------------------------------------------------------------
-- Bool

evalNot :: (HasBoolLits expr) => EvalSimpleBuiltin expr
evalNot = evalOp1 getBoolLit not mkBoolLit

evalAnd :: (HasBoolLits expr) => EvalSimpleBuiltin expr
evalAnd originalExpr = \case
  [argExpr -> IBoolLiteral _ x, argExpr -> IBoolLiteral _ y] -> IBoolLiteral mempty (x && y)
  [argExpr -> IBoolLiteral _ b, argExpr -> y] -> if b then y else IBoolLiteral mempty b
  [argExpr -> x, argExpr -> IBoolLiteral _ b] -> if b then x else IBoolLiteral mempty b
  _ -> originalExpr

evalOr :: (HasBoolLits expr) => EvalSimpleBuiltin expr
evalOr originalExpr = \case
  [argExpr -> IBoolLiteral _ x, argExpr -> IBoolLiteral _ y] -> IBoolLiteral mempty (x || y)
  [argExpr -> IBoolLiteral _ b, argExpr -> y] -> if b then IBoolLiteral mempty b else y
  [argExpr -> x, argExpr -> IBoolLiteral _ b] -> if b then IBoolLiteral mempty b else x
  _ -> originalExpr

evalIf :: (HasBoolLits expr) => EvalSimpleBuiltin expr
evalIf originalExpr = \case
  [argExpr -> IBoolLiteral _ True, e1, _e2] -> argExpr e1
  [argExpr -> IBoolLiteral _ False, _e1, e2] -> argExpr e2
  _ -> originalExpr

-- TODO define in terms of language. The problem is the polarity checking...
evalImplies :: (HasStandardData expr, HasBoolLits expr) => EvalSimpleBuiltin expr
evalImplies originalExpr = \case
  [e1, e2] -> do
    let ne1 = evalNot (INot (argExpr e1)) [e1]
    evalOr (IOr ne1 (argExpr e2)) [explicit ne1, e2]
  _ -> originalExpr

-----------------------------------------------------------------------------
-- Index

evalOrderIndex :: (HasBoolLits expr, HasIndexLits expr) => OrderOp -> EvalSimpleBuiltin expr
evalOrderIndex op originalExpr = \case
  [_, _, argExpr -> IIndexLiteral _ x, argExpr -> IIndexLiteral _ y] -> IBoolLiteral mempty (orderOp op x y)
  _ -> originalExpr

evalEqualsIndex :: (HasBoolLits expr, HasIndexLits expr) => EqualityOp -> EvalSimpleBuiltin expr
evalEqualsIndex op originalExpr = \case
  [_, _, argExpr -> IIndexLiteral _ x, argExpr -> IIndexLiteral _ y] -> IBoolLiteral mempty (equalityOp op x y)
  _ -> originalExpr

-----------------------------------------------------------------------------
-- Nat

evalAddNat :: (HasNatLits expr) => EvalSimpleBuiltin expr
evalAddNat = evalOp2 getNatLit getNatLit (+) mkNatLit

evalMulNat :: (HasNatLits expr) => EvalSimpleBuiltin expr
evalMulNat = evalOp2 getNatLit getNatLit (*) mkNatLit

evalOrderNat :: (HasBoolLits expr, HasNatLits expr) => OrderOp -> EvalSimpleBuiltin expr
evalOrderNat op = evalOp2 getNatLit getNatLit (orderOp op) mkBoolLit

evalEqualsNat :: (HasBoolLits expr, HasNatLits expr) => EqualityOp -> EvalSimpleBuiltin expr
evalEqualsNat op = evalOp2 getNatLit getNatLit (equalityOp op) mkBoolLit

evalFromNatToIndex :: (HasIndexLits expr, HasNatLits expr) => EvalSimpleBuiltin expr
evalFromNatToIndex = evalOp1 getNatLit id mkIndexLit

-----------------------------------------------------------------------------
-- Rat

evalNegRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalNegRat = evalOp1 getRatLit (\x -> -x) mkRatLit

evalAddRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalAddRat = evalOp2 getRatLit getRatLit (+) mkRatLit

evalSubRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalSubRat = evalOp2 getRatLit getRatLit (-) mkRatLit

evalMulRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalMulRat = evalOp2 getRatLit getRatLit (*) mkRatLit

evalDivRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalDivRat = evalOp2 getRatLit getRatLit (/) mkRatLit

evalMinRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalMinRat = evalOp2 getRatLit getRatLit min mkRatLit

evalMaxRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalMaxRat = evalOp2 getRatLit getRatLit max mkRatLit

evalPowRat :: (HasNatLits expr, HasRatLits expr) => EvalSimpleBuiltin expr
evalPowRat = evalOp2 getRatLit getNatLit (^^) mkRatLit

evalOrderRat :: (HasBoolLits expr, HasRatLits expr) => OrderOp -> EvalSimpleBuiltin expr
evalOrderRat op = evalOp2 getRatLit getRatLit (orderOp op) mkBoolLit

evalEqualsRat :: (HasBoolLits expr, HasRatLits expr) => EqualityOp -> EvalSimpleBuiltin expr
evalEqualsRat op = evalOp2 getRatLit getRatLit (equalityOp op) mkBoolLit

evalFromNatToNat :: EvalSimpleBuiltin expr
evalFromNatToNat originalExpr = \case
  [argExpr -> x] -> x
  _ -> originalExpr

evalFromNatToRat :: (HasRatLits expr, HasNatLits expr) => EvalSimpleBuiltin expr
evalFromNatToRat = evalOp1 getNatLit fromIntegral mkRatLit

evalFromRatToRat :: EvalSimpleBuiltin expr
evalFromRatToRat originalExpr = \case
  [argExpr -> x] -> x
  _ -> originalExpr

-----------------------------------------------------------------------------
-- From here on, these only work for standard typing systems with implicit
-- arguments in the format expected by the standard typing system, as
-- otherwise we can't reconstruct the MapList and FoldList with the right
-- typing arguments.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- List

evalMapList :: (HasStandardListLits expr) => ([GenericArg expr] -> expr) -> EvalBuiltin expr m
evalMapList mkMapList evalApp originalExpr = \case
  [_a, b, _f, argExpr -> INil _] -> return $ INil b
  [a, b, f, argExpr -> ICons _ x xs] -> do
    fx <- evalApp (argExpr f) [x]
    let defaultMap = mkMapList [a, b, f, xs]
    fxs <- evalMapList mkMapList evalApp defaultMap [a, b, f, xs]
    return $ ICons b (explicit fx) (explicit fxs)
  _ -> return originalExpr

evalFoldList :: (HasStandardListLits expr) => ([GenericArg expr] -> expr) -> EvalBuiltin expr m
evalFoldList mkFoldList evalApp originalExpr args =
  case args of
    [_a, _b, _f, e, argExpr -> INil _] -> return $ argExpr e
    [a, b, f, e, argExpr -> ICons _ x xs] -> do
      let defaultFold = mkFoldList [a, b, f, e, xs]
      r <- evalFoldList mkFoldList evalApp defaultFold [a, b, f, e, xs]
      evalApp (argExpr f) [x, explicit r]
    _ -> return originalExpr

-----------------------------------------------------------------------------
-- Vector

evalIndices :: (HasStandardVecLits expr, HasIndexLits expr, HasNatLits expr) => ([GenericArg expr] -> expr) -> EvalSimpleBuiltin expr
evalIndices mkIndexType originalExpr = \case
  [size@(argExpr -> INatLiteral _ n)] -> do
    let t = implicit (mkIndexType [size])
    let xs = fmap (explicit . IIndexLiteral mempty) ([0 .. n - 1] :: [Int])
    IVecLiteral t xs
  _ -> originalExpr

evalAt :: (HasStandardVecLits expr, HasIndexLits expr) => EvalSimpleBuiltin expr
evalAt originalExpr = \case
  [_, argExpr -> IVecLiteral _t xs, argExpr -> IIndexLiteral _ i] -> case xs !!? fromIntegral i of
    Nothing -> developerError $ "out of bounds error:" <+> pretty (length xs) <+> "<=" <+> pretty i
    Just xsi -> argExpr xsi
  _ -> originalExpr

evalFoldVector :: (HasStandardVecLits expr) => EvalBuiltin expr m
evalFoldVector evalApp originalExpr args = case args of
  [_, _, argExpr -> f, argExpr -> e, argExpr -> IVecLiteral _t xs] -> foldrM f' e xs
    where
      f' x r = evalApp f [x, explicit r]
  _ -> return originalExpr

evalZipWith :: (HasStandardVecLits expr) => EvalBuiltin expr m
evalZipWith evalApp originalExpr = \case
  [_, _, c, argExpr -> f, argExpr -> IVecLiteral _t1 xs, argExpr -> IVecLiteral _t2 ys] ->
    IVecLiteral c <$> zipWithM f' xs ys
    where
      f' x y = explicit <$> evalApp f [x, y]
  _ -> return originalExpr

evalMapVector :: (HasStandardVecLits expr) => EvalBuiltin expr m
evalMapVector evalApp originalExpr = \case
  [_, b, argExpr -> f, argExpr -> IVecLiteral _t1 xs] ->
    IVecLiteral b <$> traverse f' xs
    where
      f' x = explicit <$> evalApp f [x]
  _ -> return originalExpr

-----------------------------------------------------------------------------
-- Rational tensors

evalRatTensorBuiltin :: (HasRatTensors expr, HasDimensionData expr) => RatTensorBuiltin -> EvalSimpleBuiltin expr
evalRatTensorBuiltin b originalExpr args = case b of
  RatTensor {} -> originalExpr
  RatType {} -> originalExpr
  RatLiteral {} -> originalExpr
  SearchRatTensor {} -> originalExpr
  NegRatTensor -> evalRatTensorOp1 (\x -> -x) originalExpr args
  AddRatTensor -> evalAddRatTensor originalExpr args
  SubRatTensor -> evalRatTensorOp2 (-) originalExpr args
  MulRatTensor -> evalMulRatTensor originalExpr args
  DivRatTensor -> evalRatTensorOp2 (/) originalExpr args
  MinRatTensor -> evalMinRatTensor originalExpr args
  MaxRatTensor -> evalMaxRatTensor originalExpr args
  ReduceAddRatTensor -> evalReduceAddRatTensor originalExpr args
  ReduceMulRatTensor -> evalReduceMulRatTensor originalExpr args
  ReduceMinRatTensor -> evalReduceMinRatTensor originalExpr args
  ReduceMaxRatTensor -> evalReduceMaxRatTensor originalExpr args

evalRatTensorOp1 :: (HasRatTensors expr) => (Rational -> Rational) -> EvalSimpleBuiltin expr
evalRatTensorOp1 op = evalOp1 getRatTensor (mapTensor op) mkRatTensor

evalRatTensorOp2 :: (HasRatTensors expr) => (Rational -> Rational -> Rational) -> EvalSimpleBuiltin expr
evalRatTensorOp2 op = evalOp2 getRatTensor getRatTensor (zipWithTensor op) mkRatTensor

evalReduceRatTensor :: (HasRatTensors expr) => (Tensor Rational -> Tensor Rational -> Tensor Rational) -> EvalSimpleBuiltin expr
evalReduceRatTensor f = evalReduceTensor getRatTensor f mkRatTensor

evalAddRatTensor :: (HasRatTensors expr, HasDimensionData expr) => EvalSimpleBuiltin expr
evalAddRatTensor originalExpr = \case
  [argExpr -> e1, argExpr -> e2] -> case (e1, e2) of
    (IRatTensor x, IRatTensor y) -> IRatTensor $ zipWithTensor (+) x y
    (IRatConstTensor r _, _) | r == 0 -> e2
    (_, IRatConstTensor r _) | r == 0 -> e1
    _ -> originalExpr
  _ -> originalExpr

evalMulRatTensor :: (HasRatTensors expr, HasDimensionData expr) => EvalSimpleBuiltin expr
evalMulRatTensor originalExpr = \case
  [argExpr -> e1, argExpr -> e2] -> case (e1, e2) of
    (IRatTensor x, IRatTensor y) -> IRatTensor $ zipWithTensor (*) x y
    (IRatConstTensor r _, _) | r == 1 -> e2
    (_, IRatConstTensor r _) | r == 1 -> e1
    _ -> originalExpr
  _ -> originalExpr

evalMinRatTensor :: (HasRatTensors expr) => EvalSimpleBuiltin expr
evalMinRatTensor = evalRatTensorOp2 min

evalMaxRatTensor :: (HasRatTensors expr) => EvalSimpleBuiltin expr
evalMaxRatTensor = evalRatTensorOp2 max

evalReduceAddRatTensor :: (HasRatTensors expr) => EvalSimpleBuiltin expr
evalReduceAddRatTensor = evalReduceRatTensor (zipWithTensor (+))

evalReduceMulRatTensor :: (HasRatTensors expr) => EvalSimpleBuiltin expr
evalReduceMulRatTensor = evalReduceRatTensor (zipWithTensor (*))

evalReduceMinRatTensor :: (HasRatTensors expr) => EvalSimpleBuiltin expr
evalReduceMinRatTensor = evalReduceRatTensor (zipWithTensor min)

evalReduceMaxRatTensor :: (HasRatTensors expr) => EvalSimpleBuiltin expr
evalReduceMaxRatTensor = evalReduceRatTensor (zipWithTensor max)

-----------------------------------------------------------------------------
-- Rational tensors

evalBoolTensorBuiltin :: (Show expr, HasBoolTensors expr, HasDimensionData expr) => BoolTensorBuiltin -> EvalSimpleBuiltin expr
evalBoolTensorBuiltin b originalExpr args = case b of
  BoolType -> originalExpr
  BoolLiteral {} -> originalExpr
  BoolTensor {} -> originalExpr
  QuantifyRatTensor {} -> originalExpr
  EqualsRatTensor op -> evalEqualityRatTensor op originalExpr args
  OrderRatTensor op -> evalOrderRatTensor op originalExpr args
  AndBoolTensor -> evalAndBoolTensor originalExpr args
  OrBoolTensor -> evalOrBoolTensor originalExpr args
  NotBoolTensor -> evalNotBoolTensor originalExpr args
  ReduceAndTensor -> evalReduceAndTensor originalExpr args
  ReduceOrTensor -> evalReduceOrTensor originalExpr args

evalNotBoolTensor :: (HasBoolTensors expr) => EvalSimpleBuiltin expr
evalNotBoolTensor = evalOp1 getBoolTensor (mapTensor not) mkBoolTensor

evalAndBoolTensor :: (Show expr, HasBoolTensors expr, HasDimensionData expr) => EvalSimpleBuiltin expr
evalAndBoolTensor originalExpr = \case
  [argExpr -> e1, argExpr -> e2] -> case (e1, e2) of
    (IBoolTensor x, IBoolTensor y) -> IBoolTensor $ zipWithTensor (&&) x y
    (IBoolConstTensor b _, _) -> if b then e2 else e1
    (_, IBoolConstTensor b _) -> if b then e1 else e2
    _ -> originalExpr
  _ -> originalExpr

evalOrBoolTensor :: (HasBoolTensors expr, HasDimensionData expr) => EvalSimpleBuiltin expr
evalOrBoolTensor originalExpr = \case
  [argExpr -> e1, argExpr -> e2] -> case (e1, e2) of
    (IBoolTensor x, IBoolTensor y) -> IBoolTensor $ zipWithTensor (||) x y
    (IBoolConstTensor b _, _) -> if b then e1 else e2
    (_, IBoolConstTensor b _) -> if b then e2 else e1
    _ -> originalExpr
  _ -> originalExpr

evalEqualityRatTensor :: (HasBoolTensors expr) => EqualityOp -> EvalSimpleBuiltin expr
evalEqualityRatTensor op = evalOp2 getRatTensor getRatTensor (zipWithTensor (equalityOp op)) mkBoolTensor

evalOrderRatTensor :: (HasBoolTensors expr) => OrderOp -> EvalSimpleBuiltin expr
evalOrderRatTensor op = evalOp2 getRatTensor getRatTensor (zipWithTensor (orderOp op)) mkBoolTensor

evalReduceAndTensor :: (HasBoolTensors expr) => EvalSimpleBuiltin expr
evalReduceAndTensor = evalReduceTensor getBoolTensor (zipWithTensor (&&)) mkBoolTensor

evalReduceOrTensor :: (HasBoolTensors expr) => EvalSimpleBuiltin expr
evalReduceOrTensor = evalReduceTensor getBoolTensor (zipWithTensor (||)) mkBoolTensor

-----------------------------------------------------------------------------
-- Slices

evalDimensionTypeBuiltin :: DimensionTypeBuiltin -> EvalSimpleBuiltin expr
evalDimensionTypeBuiltin _ originalExpr _ = originalExpr

evalDimensionDataBuiltin :: DimensionDataBuiltin -> EvalSimpleBuiltin expr
evalDimensionDataBuiltin _ originalExpr _ = originalExpr -- TODO

-----------------------------------------------------------------------------
-- Type-class

-- | A type-class for builtins that can be normalised compositionally.
class (PrintableBuiltin builtin) => NormalisableBuiltin builtin where
  -- This function takes in the original expression (containing both relevant
  -- and irrelevant arguments), the builtin that is in the head position
  -- and the list of computationally relevant arguments.
  evalBuiltinApp ::
    (MonadLogger m, Show closure) =>
    EvalApp (Value closure builtin) m ->
    Value closure builtin ->
    builtin ->
    Spine closure builtin ->
    m (Value closure builtin)

  blockingArgs ::
    builtin ->
    [Int]

functionBlockingArgs :: BuiltinFunction -> [Int]
functionBlockingArgs = \case
  Quantifier {} -> []
  Not -> [0]
  And -> [0, 1]
  Or -> [0, 1]
  Neg NegRat -> [0]
  Add AddNat -> [0, 1]
  Add AddRat -> [0, 1]
  Sub SubRat -> [0, 1]
  Mul MulNat -> [0, 1]
  Mul MulRat -> [0, 1]
  Div DivRat -> [0, 1]
  PowRat -> [0, 1]
  MinRat -> [0, 1]
  MaxRat -> [0, 1]
  Equals EqIndex _op -> [2, 3]
  Equals EqNat _op -> [0, 1]
  Equals EqRat _op -> [0, 1]
  Order OrderIndex _op -> [2, 3]
  Order OrderNat _op -> [0, 1]
  Order OrderRat _op -> [0, 1]
  FromNat FromNatToIndex -> [1]
  FromNat FromNatToNat -> []
  FromNat FromNatToRat -> [0]
  FromRat FromRatToRat -> [0]
  If -> [1]
  At -> [2, 3]
  FoldVector -> [5]
  FoldList -> [4]
  ZipWithVector -> [5, 6]
  MapList -> [3]
  MapVector -> [4]
  Indices -> [0]
  Implies -> []

instance NormalisableBuiltin Builtin where
  evalBuiltinApp = evalTypeClassOp $ \b evalApp originalValue args -> case b of
    Quantifier {} -> return originalValue
    Not -> return $ evalNot originalValue args
    And -> return $ evalAnd originalValue args
    Or -> return $ evalOr originalValue args
    Neg NegRat -> return $ evalNegRat originalValue args
    Add AddNat -> return $ evalAddNat originalValue args
    Add AddRat -> return $ evalAddRat originalValue args
    Sub SubRat -> return $ evalSubRat originalValue args
    Mul MulNat -> return $ evalMulNat originalValue args
    Mul MulRat -> return $ evalMulRat originalValue args
    Div DivRat -> return $ evalDivRat originalValue args
    PowRat -> return $ evalPowRat originalValue args
    MinRat -> return $ evalMinRat originalValue args
    MaxRat -> return $ evalMaxRat originalValue args
    Equals EqIndex op -> return $ evalEqualsIndex op originalValue args
    Equals EqNat op -> return $ evalEqualsNat op originalValue args
    Equals EqRat op -> return $ evalEqualsRat op originalValue args
    Order OrderIndex op -> return $ evalOrderIndex op originalValue args
    Order OrderNat op -> return $ evalOrderNat op originalValue args
    Order OrderRat op -> return $ evalOrderRat op originalValue args
    FromNat FromNatToIndex -> return $ evalFromNatToIndex originalValue args
    FromNat FromNatToNat -> return $ evalFromNatToNat originalValue args
    FromNat FromNatToRat -> return $ evalFromNatToRat originalValue args
    FromRat FromRatToRat -> return $ evalFromRatToRat originalValue args
    If -> return $ evalIf originalValue args
    At -> return $ evalAt originalValue args
    FoldVector -> evalFoldVector evalApp originalValue args
    FoldList -> evalFoldList (VBuiltinFunction FoldList) evalApp originalValue args
    ZipWithVector -> evalZipWith evalApp originalValue args
    MapList -> evalMapList (VBuiltinFunction MapList) evalApp originalValue args
    MapVector -> evalMapVector evalApp originalValue args
    Indices -> return $ evalIndices (VBuiltin (BuiltinType Index)) originalValue args
    Implies -> return $ evalImplies originalValue args

  blockingArgs = \case
    BuiltinFunction f -> functionBlockingArgs f
    _ -> []

evalTensorBuiltin :: (Show closure) => TensorBuiltin -> EvalSimpleBuiltin (Value closure TensorBuiltin)
evalTensorBuiltin b originalExpr spine =
  case b of
    TensorRat op -> evalRatTensorBuiltin op originalExpr spine
    TensorBool op -> evalBoolTensorBuiltin op originalExpr spine
    TensorDimType op -> evalDimensionTypeBuiltin op originalExpr spine
    TensorDimData op -> evalDimensionDataBuiltin op originalExpr spine

instance NormalisableBuiltin TensorBuiltin where
  evalBuiltinApp _evalApp originalExpr b = return . evalTensorBuiltin b originalExpr

  blockingArgs = developerError "forcing not yet implemented for tensor builtins"

instance NormalisableBuiltin LossTensorBuiltin where
  evalBuiltinApp _evalApp originalExpr builtin args = case builtin of
    LossTensorRat op -> return $ evalRatTensorBuiltin op originalExpr args
    LossTensorDimType op -> return $ evalDimensionTypeBuiltin op originalExpr args
    LossTensorDimData op -> return $ evalDimensionDataBuiltin op originalExpr args

  blockingArgs = developerError "forcing not yet implemented for tensor builtins"

instance NormalisableBuiltin LinearityBuiltin where
  evalBuiltinApp = evalTypeClassOp $ \b evalApp originalValue args -> case b of
    Quantifier {} -> return originalValue
    Not -> return $ evalNot originalValue args
    And -> return $ evalAnd originalValue args
    Or -> return $ evalOr originalValue args
    Neg NegRat -> return $ evalNegRat originalValue args
    Add AddNat -> return $ evalAddNat originalValue args
    Add AddRat -> return $ evalAddRat originalValue args
    Sub SubRat -> return $ evalSubRat originalValue args
    Mul MulNat -> return $ evalMulNat originalValue args
    Mul MulRat -> return $ evalMulRat originalValue args
    Div DivRat -> return $ evalDivRat originalValue args
    PowRat -> return $ evalPowRat originalValue args
    MinRat -> return $ evalMinRat originalValue args
    MaxRat -> return $ evalMaxRat originalValue args
    Equals EqIndex op -> return $ evalEqualsIndex op originalValue args
    Equals EqNat op -> return $ evalEqualsNat op originalValue args
    Equals EqRat op -> return $ evalEqualsRat op originalValue args
    Order OrderIndex op -> return $ evalOrderIndex op originalValue args
    Order OrderNat op -> return $ evalOrderNat op originalValue args
    Order OrderRat op -> return $ evalOrderRat op originalValue args
    FromNat FromNatToIndex -> return $ evalFromNatToIndex originalValue args
    FromNat FromNatToNat -> return $ evalFromNatToNat originalValue args
    FromNat FromNatToRat -> return $ evalFromNatToRat originalValue args
    FromRat FromRatToRat -> return $ evalFromRatToRat originalValue args
    If -> return $ evalIf originalValue args
    At -> notImplemented b
    FoldVector -> notImplemented b
    FoldList -> evalLinearityFoldList evalApp originalValue args
    ZipWithVector -> notImplemented b
    MapList -> notImplemented b
    MapVector -> notImplemented b
    Indices -> notImplemented b
    Implies -> notImplemented b
    where
      notImplemented = normNotImplemented "Polarity"

  blockingArgs = \case
    LinearityFunction f -> functionBlockingArgs f
    _ -> []

-- Need foldList at the type-level to evaluate the Tensor definition
evalLinearityFoldList :: EvalBuiltin (Value closure LinearityBuiltin) m
evalLinearityFoldList evalApp originalExpr args =
  case args of
    [_a, _b, _c, _f, e, argExpr -> VBuiltin (LinearityConstructor Nil) []] -> return $ argExpr e
    [a, b, c, f, e, argExpr -> VBuiltin (LinearityConstructor Cons) [_, _, _, _, x, xs]] -> do
      let defaultFold = VBuiltin (LinearityFunction FoldList) [a, b, c, f, e, xs]
      r <- evalLinearityFoldList evalApp defaultFold [a, b, c, f, e, xs]
      evalApp (argExpr f) [x, explicit r]
    _ -> return originalExpr

instance NormalisableBuiltin PolarityBuiltin where
  evalBuiltinApp = evalTypeClassOp $ \b evalApp originalValue args -> case b of
    Quantifier {} -> return originalValue
    Not -> return $ evalNot originalValue args
    And -> return $ evalAnd originalValue args
    Or -> return $ evalOr originalValue args
    Neg NegRat -> return $ evalNegRat originalValue args
    Add AddNat -> return $ evalAddNat originalValue args
    Add AddRat -> return $ evalAddRat originalValue args
    Sub SubRat -> return $ evalSubRat originalValue args
    Mul MulNat -> return $ evalMulNat originalValue args
    Mul MulRat -> return $ evalMulRat originalValue args
    Div DivRat -> return $ evalDivRat originalValue args
    PowRat -> return $ evalPowRat originalValue args
    MinRat -> return $ evalMinRat originalValue args
    MaxRat -> return $ evalMaxRat originalValue args
    Equals EqIndex op -> return $ evalEqualsIndex op originalValue args
    Equals EqNat op -> return $ evalEqualsNat op originalValue args
    Equals EqRat op -> return $ evalEqualsRat op originalValue args
    Order OrderIndex op -> return $ evalOrderIndex op originalValue args
    Order OrderNat op -> return $ evalOrderNat op originalValue args
    Order OrderRat op -> return $ evalOrderRat op originalValue args
    FromNat FromNatToIndex -> return $ evalFromNatToIndex originalValue args
    FromNat FromNatToNat -> return $ evalFromNatToNat originalValue args
    FromNat FromNatToRat -> return $ evalFromNatToRat originalValue args
    FromRat FromRatToRat -> return $ evalFromRatToRat originalValue args
    If -> return $ evalIf originalValue args
    At -> notImplemented b
    FoldVector -> notImplemented b
    FoldList -> evalPolarityFoldList evalApp originalValue args
    ZipWithVector -> notImplemented b
    MapList -> notImplemented b
    MapVector -> notImplemented b
    Indices -> notImplemented b
    Implies -> notImplemented b
    where
      notImplemented = normNotImplemented "Polarity"

  blockingArgs = \case
    PolarityFunction f -> functionBlockingArgs f
    _ -> []

-- Need foldList at the type-level to evaluate the Tensor definition
evalPolarityFoldList :: EvalBuiltin (Value closure PolarityBuiltin) m
evalPolarityFoldList evalApp originalExpr args =
  case args of
    [_a, _b, _c, _f, e, argExpr -> VBuiltin (PolarityConstructor Nil) []] -> return $ argExpr e
    [a, b, c, f, e, argExpr -> VBuiltin (PolarityConstructor Cons) [_, _, _, _, x, xs]] -> do
      let defaultFold = VBuiltin (PolarityFunction FoldList) [a, b, c, f, e, xs]
      r <- evalPolarityFoldList evalApp defaultFold [a, b, c, f, e, xs]
      evalApp (argExpr f) [x, explicit r]
    _ -> return originalExpr

normNotImplemented :: (Pretty fn) => Doc () -> fn -> a
normNotImplemented typeSystem b = developerError $ "Normalisation of " <+> pretty b <+> "at the type-level not yet supported for" <+> typeSystem <+> "system"
