--------------------------------------------------------------------------------
-- Inputs and outputs

type InputVector = Tensor Rat [2]

currentSensor  = 0
previousSensor = 1

type OutputVector = Tensor Rat [1]

velocity = 0

--------------------------------------------------------------------------------
-- Network

@network
controller : InputVector -> OutputVector

-- Normalises the input values from the range [-4, 4] metres to the range [0,1]
normalise : InputVector -> InputVector
normalise x = foreach i . (x ! i + 4.0) / 8.0

--------------------------------------------------------------------------------
-- Safety property

safeInput : InputVector -> Bool
safeInput x = forall i . -3.25 <= x ! i <= 3.25

safeOutput : InputVector -> Bool
safeOutput x = let y = controller (normalise x) ! velocity in
  -1.25 < y + 2 * (x ! currentSensor) - (x ! previousSensor) < 1.25

@property
safe : Bool
safe = forall x . safeInput x => safeOutput x
