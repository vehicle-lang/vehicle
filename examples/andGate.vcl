-- Correctness conditions for the boolean AND gate

network f : Tensor Real [2] -> Tensor Real [1]

truthy : Real -> Bool
truthy x = x >= 0.5

falsey : Real -> Bool
falsey x = x <= 0.5

validInput : Tensor Real [2] -> Bool
validInput x = forall xi : x. 0 <= xi && xi <= 1

correctOutput : Tensor Real [2] -> Bool
correctOutput x = let y = f x ! 0 in
  (truthy x!0 && falsey x!1 => truthy y) &&
  (truthy x!0 && truthy x!1 => truthy y) &&
  (falsey x!0 && falsey x!1 => truthy y) &&
  (falsey x!0 && truthy x!1 => truthy y)

correct : Bool
correct = forall x : validInput. correctOutput x
