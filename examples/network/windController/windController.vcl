type InputVector = Tensor Rat [2]

network controller : InputVector -> Rat

currentPosition : InputVector -> Rat
currentPosition x = x ! 0

previousPosition : InputVector -> Rat
previousPosition x = x ! 1

safeInput : InputVector -> Prop
safeInput x = -3.25 <= currentPosition x <= 3.25 and
              -3.25 <= previousPosition x <= 3.25

safeOutput : InputVector -> Prop
safeOutput x = -1.25 < controller x + 2 * currentPosition x - previousPosition x < 1.25

safe : Prop
safe = forall x . safeInput x => safeOutput x