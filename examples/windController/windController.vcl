--------------------------------------------------------------------------------
-- Inputs and outputs

type InputVector = Tensor Rat [2]

currentSensor  = 0
previousSensor = 1

--------------------------------------------------------------------------------
-- Network

network controller : InputVector -> Rat

--------------------------------------------------------------------------------
-- Safety property

safeInput : InputVector -> Prop
safeInput x = forall i . -3.25 <= x ! i <= 3.25

safeOutput : InputVector -> Prop
safeOutput x = -1.25 < controller x + 2 * (x ! currentSensor) - (x ! previousSensor) < 1.25

safe : Prop
safe = forall x . safeInput x => safeOutput x