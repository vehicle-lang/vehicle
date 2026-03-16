-- Correctness conditions for the Boolean AND gate

@network
andGate : Tensor Real [2] -> Tensor Real [1]

truthy : Real -> Bool
truthy x = x >= 0.5

falsey : Real -> Bool
falsey x = x <= 0.5

validInput : Real -> Bool
validInput x = 0 <= x <= 1

correctOutput : Real -> Real -> Bool
correctOutput x1 x2 =
  let y = andGate [x1, x2] ! 0 in
    (truthy x1 and truthy x2 => truthy y) and
    (truthy x1 and falsey x2 => falsey y) and
    (falsey x1 and truthy x2 => falsey y) and
    (falsey x1 and falsey x2 => falsey y)

@property
andGateCorrect : Bool
andGateCorrect = forall x1 x2 . validInput x1 and validInput x2 => correctOutput x1 x2
