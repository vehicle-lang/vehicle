-- Correctness conditions for the Boolean AND gate

network andGate : Tensor Real [2] -> Tensor Real [1]

truthy : Real -> Prop
truthy x = x >= 0.5

falsey : Real -> Prop
falsey x = x <= 0.5

validInput : Tensor Real [2] -> Prop
validInput x = every i inn ([0,1] : List Nat) . 0 <= x ! i and x ! i <= 1

correctOutput : Tensor Real [2] -> Prop
correctOutput x =
  let y = andGate x in
    (truthy (x!0) and truthy (x!1) => truthy (y!0)) and
    (truthy (x!0) and falsey (x!1) => falsey (y!0)) and
    (falsey (x!0) and truthy (x!1) => falsey (y!0)) and
    (falsey (x!0) and falsey (x!1) => falsey (y!0))

correct : Prop
correct = every x . validInput x => correctOutput x

