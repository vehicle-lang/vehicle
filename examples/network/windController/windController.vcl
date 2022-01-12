type InputVector = Tensor Rat [2]

network deltaV : InputVector -> Rat

currentPosition : InputVector -> Rat
currentPosition x = x ! 0

prevPosition : InputVector -> Rat
prevPosition x = x ! 1

safeInput : InputVector -> Prop
safeInput x = -3 <= currentPosition x <= 3

safeOutput : InputVector -> Prop
safeOutput x = -2 < deltaV x + (2 * currentPosition x) - prevPosition x < 2

safe : Prop
safe = every x . safeInput x => safeOutput x