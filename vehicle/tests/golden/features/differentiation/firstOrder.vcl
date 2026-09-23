@tensor
record Input =
  { position : Real
  , time : Real
  }

type Output = Real

@network
model : Input -> Output

timeDerivative : Input -> Output
timeDerivative input =
  delta[model]/delta[time] input

-- model                           : Input -> Real
-- delta[model]/delta[time]        : Input -> Real
-- delta[model]/delta[time] input  : Real
