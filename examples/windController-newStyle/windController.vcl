--------------------------------------------------------------------------------
-- Inputs and outputs

@tensor
record Input where
  { currentSensor  : Real
  , previousSensor : Real
  }

@tensor
record Output where
  { deltaVelocity : Real
  }

--------------------------------------------------------------------------------
-- Network

@network
controller : Input -> Output

--------------------------------------------------------------------------------
-- Safety property

safeInput : Input -> Bool
safeInput x =
  -3.25 <= x.currentSensor  <= 3.25 and
  -3.25 <= x.previousSensor <= 3.25

safeOutput : Input -> Bool
safeOutput x = -1.25 < (controller x).deltaVelocity + 2 * x.currentSensor - x.previousSensor < 1.25

@property
safe : Bool
safe = forall x . safeInput x => safeOutput x
