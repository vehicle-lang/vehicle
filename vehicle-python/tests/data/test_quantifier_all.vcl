quantifierForall : Bool
quantifierForall = forall (x : Rat) . x >= 0

@property
prop : Bool
prop = quantifierForall == False
