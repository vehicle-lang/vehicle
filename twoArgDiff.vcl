record Input where {
    position : Real,
    time : Real
}

type Output = Real

@network
model : Input -> Output

derivative : Input
derivative = delta[position]/delta[time]
