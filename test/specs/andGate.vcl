-- Correctness conditions for the Boolean AND gate

network andGate : Rat -> Rat -> Rat

truthy : Rat -> Bool
truthy x = x >= 0.5

falsey : Rat -> Bool
falsey x = x <= 0.5

validInput : Rat -> Bool
validInput x = 0 <= x <= 1

correctOutput : Rat -> Rat -> Bool
correctOutput x1 x2 =
  let y = andGate x1 x2 in
    (truthy x1 and truthy x2 => truthy y) and
    (truthy x1 and falsey x2 => falsey y) and
    (falsey x1 and truthy x2 => falsey y) and
    (falsey x1 and falsey x2 => falsey y)

andGateCorrect : Bool
andGateCorrect = forall x1 x2 . validInput x1 and validInput x2 => correctOutput x1 x2