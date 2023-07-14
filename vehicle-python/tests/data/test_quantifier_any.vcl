quantifierExists : Bool
quantifierExists = exists (x : Rat) . x >= 0

@property
prop : Bool
prop = quantifierExists == True
