@tensor
record Input = { time : Real }

@network
model : Input -> Real

derivativeEquation : Bool
derivativeEquation =
    forall i. delta[model]/delta[time] i == model i

initialCondition : Bool
initialCondition =
    model { time = 0 } == 1

@property
exponential : Bool
exponential = derivativeEquation and initialCondition
