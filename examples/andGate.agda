-- Correctness conditions for the boolean AND gate

network f : Tensor Real [2] -> Tensor Real [1]

truthy : Real -> Prop
truthy x = x >= 0.5

falsey : Real -> Prop
falsey x = x <= 0.5

validInput : Tensor Real [2] -> Prop
validInput x = every i in [0,1] . 0 <= x ! i and x ! i <= 1

correctOutput : Tensor Real [2] -> Prop
correctOutput x =
  let y : Real
      y = f x ! 0
  in (truthy x!0 and falsey x!1 => truthy y) and
     (truthy x!0 and truthy x!1 => truthy y) and
     (falsey x!0 and falsey x!1 => truthy y) and
     (falsey x!0 and truthy x!1 => truthy y)

correct : Prop
correct = every x . validInput x => correctOutput x