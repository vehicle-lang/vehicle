module Vehicle.Backend.Solver.UserVariableElimination.LinearExpr
  ( LinearityError (..),
    compileLinearAssertion,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Applicative (Applicative (..))
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Trans (MonadTrans (..))
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion (comparisonToAssertion)
import Vehicle.Data.Builtin.Standard.Core (Builtin, ComparisonOp)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.MaybeTrivial (trivialElim)
import Vehicle.Data.Tensor (TensorShape, toFiniteRatTensor, pattern ConstantTensor)
import Vehicle.Data.Variable.Bound.Level
import Prelude hiding (Applicative (..))

type MonadCompileLinearExpr m =
  ( MonadLogger m,
    MonadError LinearityError m
  )

data LinearityError
  = NonLinearity
  | UnexpectedExpr (Value Builtin)
  | UnreducedExpr (Value Builtin)
  | TrivialExpr Bool

--------------------------------------------------------------------------------
-- Tensor expression

compileLinearAssertion ::
  (MonadLogger m) =>
  (Lv -> m SliceVariable) ->
  ComparisonOp ->
  TensorShape ->
  Value Builtin ->
  Value Builtin ->
  m (Either LinearityError LinearAssertion)
compileLinearAssertion toVar op shape x y = do
  runExceptT $ do
    linX <- compile (lift . toVar) shape x
    linY <- compile (lift . toVar) shape y
    boolOrAssertion <- comparisonToAssertion op linX linY
    trivialElim (throwError . TrivialExpr) return boolOrAssertion

compile ::
  forall m.
  (MonadCompileLinearExpr m) =>
  (Lv -> m SliceVariable) ->
  TensorShape ->
  Value Builtin ->
  m LinearExpression
compile toVar shape = go
  where
    go :: Value Builtin -> m LinearExpression
    go expr = case toRatTensorValue expr of
      ----------------
      -- Base cases --
      ----------------
      VRatTensorLiteral tensor -> case toFiniteRatTensor tensor of
        Nothing -> developerError "Infinite values not supported in query backend"
        Just finiteTensor -> return $ constantExpr finiteTensor
      VRatTensorBoundVar lv -> do
        singletonVarExpr (ConstantTensor shape 0) <$> toVar lv
      ---------------------
      -- Inductive cases --
      ---------------------
      VNegRatTensor (TensorOp1Args _ e) -> scaleExpr (-1) <$> go e
      VAddRatTensor (TensorOp2Args _ e1 e2) -> addExprsUnsafe 1 1 <$> go e1 <*> go e2
      VSubRatTensor (TensorOp2Args _ e1 e2) -> addExprsUnsafe 1 (-1) <$> go e1 <*> go e2
      VMulRatTensor (TensorOp2Args _ e1 e2) -> do
        e1' <- compile toVar shape e1
        e2' <- compile toVar shape e2
        case (isConstant e1', isConstant e2') of
          (Just (ConstantTensor _ c1), _) -> return $ scaleExpr c1 e2'
          (_, Just (ConstantTensor _ c2)) -> return $ scaleExpr c2 e1'
          (Just _, _) -> unreduced
          (_, Just _) -> unreduced
          _ -> throwError NonLinearity
      VDivRatTensor (TensorOp2Args _ e1 e2) -> do
        e1' <- compile toVar shape e1
        e2' <- compile toVar shape e2
        case isConstant e2' of
          Just (ConstantTensor _ c2) -> return $ scaleExpr (1 / c2) e1'
          Just _ -> unreduced
          _ -> throwError NonLinearity
      VPowRatTensor {} -> throwError NonLinearity
      VLogRatTensor {} -> throwError NonLinearity
      VExpRatTensor {} -> throwError NonLinearity
      ---------------------
      -- Unreduced cases --
      ---------------------
      -- The expression is being blocked
      VRatConstTensor {} -> unreduced
      VRatStackTensor {} -> unreduced
      VRatAtTensor {} -> unreduced
      VRatAtVector {} -> unreduced
      VRatTensorNetworkApp {} -> unreduced
      VDatasetOrParameter {} -> unreduced
      VRatRecordAcc {} -> unreduced
      VRatForeach {} -> unreduced
      VIfRatTensor {} -> unreduced
      -----------------------
      -- Unsupported cases --
      -----------------------
      VMinRatTensor {} -> unexpected
      VMaxRatTensor {} -> unexpected
      VReduceAddRatTensor {} -> unexpected
      VReduceMulRatTensor {} -> unexpected
      VReduceMinRatTensor {} -> unexpected
      VReduceMaxRatTensor {} -> unexpected
      where
        unexpected = throwError $ UnexpectedExpr expr
        unreduced = throwError $ UnreducedExpr expr
