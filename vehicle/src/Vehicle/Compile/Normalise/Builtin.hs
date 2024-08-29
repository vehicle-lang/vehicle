{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.Builtin where

import Control.Monad (zipWithM)
import Data.Foldable (foldrM)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface (BuiltinHasStandardData (..))
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
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

-- | A type-class for builtins that can be normalised compositionally.
class (PrintableBuiltin builtin) => NormalisableBuiltin builtin where
  -- This function takes in the original expression (containing both relevant
  -- and irrelevant arguments), the builtin that is in the head position
  -- and the list of computationally relevant arguments.
  evalBuiltinApp ::
    (MonadLogger m) =>
    EvalApp (Value closure builtin) m ->
    Value closure builtin ->
    builtin ->
    Spine closure builtin ->
    m (Value closure builtin)

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

-----------------------------------------------------------------------------
-- Individual builtin evaluation
-----------------------------------------------------------------------------
-- Bool

evalNot :: (HasBoolLits expr) => EvalSimpleBuiltin expr
evalNot originalExpr = \case
  [argExpr -> IBoolLiteral _ x] -> IBoolLiteral mempty (not x)
  _ -> originalExpr

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
evalAddNat originalExpr = \case
  [argExpr -> INatLiteral _ x, argExpr -> INatLiteral _ y] -> INatLiteral mempty (x + y)
  _ -> originalExpr

evalMulNat :: (HasNatLits expr) => EvalSimpleBuiltin expr
evalMulNat originalExpr = \case
  [argExpr -> INatLiteral _ x, argExpr -> INatLiteral _ y] -> INatLiteral mempty (x * y)
  _ -> originalExpr

evalOrderNat :: (HasBoolLits expr, HasNatLits expr) => OrderOp -> EvalSimpleBuiltin expr
evalOrderNat op originalExpr = \case
  [argExpr -> INatLiteral _ x, argExpr -> INatLiteral _ y] -> IBoolLiteral mempty (orderOp op x y)
  _ -> originalExpr

evalEqualsNat :: (HasBoolLits expr, HasNatLits expr) => EqualityOp -> EvalSimpleBuiltin expr
evalEqualsNat op originalExpr = \case
  [argExpr -> INatLiteral _ x, argExpr -> INatLiteral _ y] -> IBoolLiteral mempty (equalityOp op x y)
  _ -> originalExpr

evalFromNatToIndex :: (HasIndexLits expr, HasNatLits expr) => EvalSimpleBuiltin expr
evalFromNatToIndex originalExpr = \case
  [argExpr -> INatLiteral _ x] -> IIndexLiteral mempty x
  _ -> originalExpr

-----------------------------------------------------------------------------
-- Rat

evalNegRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalNegRat originalExpr = \case
  [argExpr -> IRatLiteral _ x] -> IRatLiteral mempty (-x)
  _ -> originalExpr

evalAddRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalAddRat originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IRatLiteral mempty (x + y)
  _ -> originalExpr

evalSubRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalSubRat originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IRatLiteral mempty (x - y)
  _ -> originalExpr

evalMulRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalMulRat originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IRatLiteral mempty (x * y)
  _ -> originalExpr

evalDivRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalDivRat originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IRatLiteral mempty (x / y)
  _ -> originalExpr

evalPowRat :: (HasNatLits expr, HasRatLits expr) => EvalSimpleBuiltin expr
evalPowRat originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> INatLiteral _ y] -> IRatLiteral mempty (x ^^ y)
  _ -> originalExpr

evalMinRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalMinRat originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IRatLiteral mempty (min x y)
  _ -> originalExpr

evalMaxRat :: (HasRatLits expr) => EvalSimpleBuiltin expr
evalMaxRat originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IRatLiteral mempty (max x y)
  _ -> originalExpr

evalOrderRat :: (HasBoolLits expr, HasRatLits expr) => OrderOp -> EvalSimpleBuiltin expr
evalOrderRat op originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IBoolLiteral mempty (orderOp op x y)
  _ -> originalExpr

evalEqualsRat :: (HasBoolLits expr, HasRatLits expr) => EqualityOp -> EvalSimpleBuiltin expr
evalEqualsRat op originalExpr = \case
  [argExpr -> IRatLiteral _ x, argExpr -> IRatLiteral _ y] -> IBoolLiteral mempty (equalityOp op x y)
  _ -> originalExpr

evalFromNatToNat :: EvalSimpleBuiltin expr
evalFromNatToNat originalExpr = \case
  [argExpr -> x] -> x
  _ -> originalExpr

evalFromNatToRat :: (HasRatLits expr, HasNatLits expr) => EvalSimpleBuiltin expr
evalFromNatToRat originalExpr = \case
  [argExpr -> INatLiteral _ x] -> IRatLiteral mempty (fromIntegral x)
  _ -> originalExpr

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
