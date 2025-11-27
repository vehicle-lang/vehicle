neg : Tensor Bool [2] -> Bool
neg x = True

calc : Tensor Real [2] -> Real
calc x = 1

safe : Bool
safe = neg [forall x . calc x <= 0, True]
