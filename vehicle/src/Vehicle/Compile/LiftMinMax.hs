module Vehicle.Compile.LiftMinMax
  ( liftMinMax,
  )
where

import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView (RatTensorValue (..), fromRatTensorValue, toRatTensorValue)
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- If lifting

liftMinMax ::
  (Monad m) =>
  Value Builtin ->
  (Value Builtin -> m (Value Builtin)) ->
  m (Value Builtin)
liftMinMax (toRatTensorValue -> VMinRatTensor args) k =
  fromRatTensorValue . VMinRatTensor <$> traverseTensorOp2Args k args
liftMinMax e k = k e
