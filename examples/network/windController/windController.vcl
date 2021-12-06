type InputVector  = Tensor Rat [2]
type OutputVector = Tensor Rat [1]

network controller : InputVector -> OutputVector

abs : Rat -> Rat
abs x = if x < 0 then - x else x

currentPosition : InputVector -> Rat
currentPosition x = x ! 0

prevPosition : InputVector -> Rat
prevPosition x = x ! 1

deltaV : InputVector -> Rat
deltaV x = controller x ! 0

safeInput : InputVector -> Prop
safeInput x = abs (currentPosition x) <= 3

safeOutput : InputVector -> Prop
safeOutput x = abs (deltaV x + (2 * currentPosition x) - prevPosition x) <= 2

safe : Prop
safe = every x . safeInput x => safeOutput x