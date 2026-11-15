record Input where {
    position : Real,
    time : Real
}

type Output = Real
type Model = Input -> Output

@network
model : Model

firstOrderDerivative : Model
firstOrderDerivative = delt[model]/delt[time]

secondOrderDerivative : Model
secondOrderDerivative = delt[firstOrderDerivative]/delt[time]

firstOrderDerivativeAt : Output
firstOrderDerivativeAt = delt[model]/delt[time] ({position = 1, time = 0})

secondOrderDerivativeAt : Output
secondOrderDerivativeAt = secondOrderDerivative ({position = 1, time = 0})
