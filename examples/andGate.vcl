-- Correctness conditions for the boolean AND gate

network f : Tensor Real [2] -> Tensor Real [1]

truthy : Real -> Bool
truthy x = x >= 0.5

falsey : Real -> Bool
falsey x = x <= 0.5

validInput : Tensor Real [2] -> Bool
validInput x = all (\xi -> 0 <= xi and xi <= 1) x

correctOutput : Tensor Real [2] -> Bool
correctOutput x =
  let y : Real
      y = f x ! 0
  in (truthy x!0 and falsey x!1 => truthy y) and
     (truthy x!0 and truthy x!1 => truthy y) and
     (falsey x!0 and falsey x!1 => truthy y) and
     (falsey x!0 and truthy x!1 => truthy y)

correct : Bool
correct = all correctOutput validInput
