type InputVector = Tensor Rat [2]

network controller : InputVector -> Rat

currentSensor : InputVector -> Rat
currentSensor x = x ! 0

previousSensor : InputVector -> Rat
previousSensor x = x ! 1

safeInput : InputVector -> Prop
safeInput x = -3.25 <= currentSensor  x <= 3.25 and
              -3.25 <= previousSensor x <= 3.25

safeOutput : InputVector -> Prop
safeOutput x = -1.25 < controller x + 2 * currentSensor x - previousSensor x < 1.25

safe : Prop
safe = forall x . safeInput x => safeOutput x