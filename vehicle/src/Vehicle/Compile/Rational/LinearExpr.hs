module Vehicle.Compile.Rational.LinearExpr
  ( LinearityError (..),
    compileLinearRelation,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Applicative (Applicative (..))
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Trans (MonadTrans (..))
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr (LinearExpr, VariableLike, addExprs, constantExpr, isConstant, scaleExpr, singletonVarExpr)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (RatTensor, TensorShape, pattern ConstantTensor, pattern ZeroDimTensor)
import Prelude hiding (Applicative (..))

type MonadCompileLinearExpr m =
  ( MonadLogger m,
    MonadError LinearityError m
  )

data LinearityError
  = NonLinearity
  | UnexpectedExpr (Value Builtin)
  | UnreducedExpr (Value Builtin)

--------------------------------------------------------------------------------
-- Tensor expression

compileLinearRelation ::
  (MonadLogger m, VariableLike variable) =>
  (Lv -> m variable) ->
  TensorShape ->
  Value Builtin ->
  Value Builtin ->
  m (Either LinearityError (LinearExpr variable RatTensor, LinearExpr variable RatTensor))
compileLinearRelation toVar shape x y = do
  runExceptT $ do
    x' <- compile (lift . toVar) shape x
    y' <- compile (lift . toVar) shape y
    return (x', y')

compile ::
  forall m variable.
  (MonadCompileLinearExpr m, VariableLike variable) =>
  (Lv -> m variable) ->
  TensorShape ->
  Value Builtin ->
  m (LinearExpr variable RatTensor)
compile toVar shape = go
  where
    go :: Value Builtin -> m (LinearExpr variable RatTensor)
    go expr = case toRatTensorValue expr of
      ----------------
      -- Base cases --
      ----------------
      VRatTensorLiteral t -> do
        return $ constantExpr t
      VRatTensorVar lv -> do
        singletonVarExpr (ConstantTensor shape 0) <$> toVar lv
      ---------------------
      -- Inductive cases --
      ---------------------
      VNegRatTensor (TensorOp1Args _ e) -> scaleExpr (-1) <$> go e
      VAddRatTensor (TensorOp2Args _ e1 e2) -> addExprs 1 1 <$> go e1 <*> go e2
      VSubRatTensor (TensorOp2Args _ e1 e2) -> addExprs 1 (-1) <$> go e1 <*> go e2
      VMulRatTensor (TensorOp2Args _ e1 e2) -> do
        e1' <- compile toVar shape e1
        e2' <- compile toVar shape e2
        case (isConstant e1', isConstant e2') of
          (Just (ZeroDimTensor c1), _) -> return $ scaleExpr c1 e2'
          (_, Just (ZeroDimTensor c2)) -> return $ scaleExpr c2 e1'
          _ -> throwError NonLinearity
      VDivRatTensor (TensorOp2Args _ e1 e2) -> do
        e1' <- compile toVar shape e1
        e2' <- compile toVar shape e2
        case isConstant e2' of
          (Just (ZeroDimTensor c2)) -> return $ scaleExpr (1 / c2) e1'
          _ -> throwError NonLinearity
      ---------------------
      -- Unreduced cases --
      ---------------------
      -- The expression is being blocked
      VRatConstTensor {} -> unreduced
      VRatStackTensor {} -> unreduced
      VRatAt {} -> unreduced
      VNetworkApp {} -> unreduced
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
