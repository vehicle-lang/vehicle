type InputVector = Tensor Rat [2]

network deltaV : InputVector -> Rat

currPosition : InputVector -> Rat
currPosition x = x ! 0

prevPosition : InputVector -> Rat
prevPosition x = x ! 1

safeInput : InputVector -> Prop
safeInput x = -3.25 <= currPosition x <= 3.25 and -3.25 <= prevPosition x <= 3.25

safeOutput : InputVector -> Prop
safeOutput x = -1.25 < deltaV x + 2 * currPosition x - prevPosition x < 1.25

safe : Prop
safe = every x . safeInput x => safeOutput x