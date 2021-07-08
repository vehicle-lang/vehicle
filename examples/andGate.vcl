-- Correctness conditions for the Propean AND gate

network andGate : Tensor Real [2] -> Real

truthy : Real -> Prop
truthy x = x >= 0.5

falsey : Real -> Prop
falsey x = x <= 0.5

validInput : Tensor Real [2] -> Prop
validInput x = all (\xi -> 0 <= xi and xi <= 1) x

correctOutput : Tensor Real [2] -> Prop
correctOutput x =
  let y : Real
      y = andGate x
  in (truthy x!0 and falsey x!1 => truthy y) and
     (truthy x!0 and truthy x!1 => truthy y) and
     (falsey x!0 and falsey x!1 => truthy y) and
     (falsey x!0 and truthy x!1 => truthy y)

correct : Prop
correct = all {Tensor Real [2]} correctOutput validInput
