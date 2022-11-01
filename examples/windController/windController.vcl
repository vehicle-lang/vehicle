--------------------------------------------------------------------------------
-- Inputs and outputs

type InputVector = Tensor Rat [2]

currentSensor  = 0
previousSensor = 1

--------------------------------------------------------------------------------
-- Network

@network
controller : InputVector -> Tensor Rat [1]

--------------------------------------------------------------------------------
-- Safety property

safeInput : InputVector -> Bool
safeInput x = forall i . -3.25 <= x ! i <= 3.25

safeOutput : InputVector -> Bool
safeOutput x = -1.25 < controller x ! 0 + 2 * (x ! currentSensor) - (x ! previousSensor) < 1.25

@property
safe : Bool
safe = forall x . safeInput x => safeOutput x
