module Vehicle.Compile.Rational.LinearExpr
  ( LinearityError (..),
    compileRatLinearRelation,
    compileTensorLinearRelation,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Applicative (Applicative (..))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr (LinearExpr, addExprs, constantExpr, isConstant, scaleExpr, singletonVarExpr)
import Vehicle.Data.Code.Value
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RationalTensor, Tensor (..), TensorShape, zeroTensor)
import Prelude hiding (Applicative (..))

type MonadCompileLinearExpr m =
  ( MonadLogger m,
    MonadError LinearityError m
  )

data LinearityError
  = NonLinearity
  | UnhandlableExpr (WHNFValue Builtin)

--------------------------------------------------------------------------------
-- Rational expression

compileRatLinearRelation ::
  (MonadLogger m) =>
  (Lv -> ExceptT LinearityError m ElementVariable) ->
  (LinearExpr ElementVariable Rational -> LinearExpr ElementVariable Rational -> relation) ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (Either LinearityError relation)
compileRatLinearRelation handleVar mkRelation x y = do
  runExceptT $ do
    x' <- compileRatLinearExpr handleVar x
    y' <- compileRatLinearExpr handleVar y
    return $ mkRelation x' y'

compileRatLinearExpr ::
  forall m.
  (MonadCompileLinearExpr m) =>
  (Lv -> m ElementVariable) ->
  WHNFValue Builtin ->
  m (LinearExpr ElementVariable Rational)
compileRatLinearExpr handleVar = go
  where
    go :: WHNFValue Builtin -> m (LinearExpr ElementVariable Rational)
    go e = case e of
      ----------------
      -- Base cases --
      ----------------
      IRatLiteral _ l -> return $ constantExpr l
      VBoundVar lv [] -> singletonVarExpr 0 <$> handleVar lv
      ---------------------
      -- Inductive cases --
      ---------------------
      INeg NegRat v -> scaleExpr (-1) <$> go v
      IAdd AddRat e1 e2 -> addExprs 1 1 <$> go e1 <*> go e2
      ISub SubRat e1 e2 -> addExprs 1 (-1) <$> go e1 <*> go e2
      IMul MulRat e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        case (isConstant e1', isConstant e2') of
          (Just c1, _) -> return $ scaleExpr c1 e2'
          (_, Just c2) -> return $ scaleExpr c2 e1'
          _ -> throwError NonLinearity
      IDiv DivRat e1 e2 -> do
        e1' <- go e1
        e2' <- go e2
        case isConstant e2' of
          (Just c2) -> return $ scaleExpr (1 / c2) e1'
          _ -> throwError NonLinearity
      -----------------
      -- Error cases --
      -----------------
      _ -> throwError $ UnhandlableExpr e

--------------------------------------------------------------------------------
-- Tensor expression

compileTensorLinearRelation ::
  (MonadLogger m) =>
  (Lv -> ExceptT LinearityError m TensorVariable) ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (Either LinearityError (Maybe (LinearExpr TensorVariable RationalTensor, LinearExpr TensorVariable RationalTensor)))
compileTensorLinearRelation handleVar x y = do
  runExceptT $ do
    x' <- compileTensorLinearExpr handleVar x
    y' <- compileTensorLinearExpr handleVar y
    return $ liftA2 (,) x' y'

compileTensorLinearExpr ::
  forall m.
  (MonadCompileLinearExpr m) =>
  (Lv -> m TensorVariable) ->
  WHNFValue Builtin ->
  m (Maybe (LinearExpr TensorVariable RationalTensor))
compileTensorLinearExpr handleVar = go
  where
    go :: WHNFValue Builtin -> m (Maybe (LinearExpr TensorVariable RationalTensor))
    go e = case e of
      ---------------------
      -- Inductive cases --
      ---------------------
      IVectorAdd _ _ _ _ _ e1 e2 -> liftA2 (addExprs 1 1) <$> go e1 <*> go e2
      IVectorSub _ _ _ _ _ e1 e2 -> liftA2 (addExprs 1 (-1)) <$> go e1 <*> go e2
      ----------------
      -- Base cases --
      ----------------
      IVecLiteral {} -> do
        return (constantExpr <$> getRationalTensor e)
      VBoundVar lv [] -> do
        var <- handleVar lv
        return $ Just $ singletonVarExpr (zeroTensor $ tensorVarDimensions var) var
      _ -> return Nothing

getRationalTensor :: WHNFValue Builtin -> Maybe RationalTensor
getRationalTensor expr = uncurry Tensor <$> go expr
  where
    go :: WHNFValue Builtin -> Maybe (TensorShape, Vector Rational)
    go = \case
      IRatLiteral _ r -> Just ([], Vector.singleton (fromRational r))
      IVecLiteral _ xs -> do
        r <- traverse (go . argExpr) xs
        let (dims, rs) = unzip r
        case dims of
          [] -> Nothing
          (ds : _) -> Just (length xs : ds, mconcat rs)
      _ -> Nothing
