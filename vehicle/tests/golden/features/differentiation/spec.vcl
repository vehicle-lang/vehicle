record Input where {
    position : Real,
    time : Real
}

type Output = Real
type Model = Input -> Output

@network
model : Model

firstOrderDerivative : Model
firstOrderDerivative = delta[model]/delta[time]

secondOrderDerivative : Model
secondOrderDerivative = delta[firstOrderDerivative]/delta[time]

firstOrderDerivativeAt : Output
firstOrderDerivativeAt = delta[model]/delta[time] ({position = 1, time = 0})

secondOrderDerivativeAt : Output
secondOrderDerivativeAt = secondOrderDerivative ({position = 1, time = 0})
