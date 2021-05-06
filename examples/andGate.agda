-- Correctness conditions for the boolean AND gate

f : Tensor Real [2] -> Tensor Real [1]
f = evaluate _ _

truthy : Real -> Set
truthy x = x >= 0.5

falsey : Real -> Set
falsey x = x <= 0.5

validInput : Tensor Real [2] -> Set
validInput x = All (λ xi -> 0 <= xi ∧ xi <= 1) x

correctOutput : Tensor Real [2] -> Set
correctOutput x =
  let y : Real
      y = f x ! 0
  in (truthy x!0 and falsey x!1 => truthy y) and
     (truthy x!0 and truthy x!1 => truthy y) and
     (falsey x!0 and falsey x!1 => truthy y) and
     (falsey x!0 and truthy x!1 => truthy y)

correct : ∀ x -> validInput x -> correctOutput x
correct = prove _ _
