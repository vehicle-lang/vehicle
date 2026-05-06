@network
ctrl : Tensor Real [2] -> Tensor Real [1]

@dynamics
dyn : Tensor Real [2] -> Tensor Real [1] -> Tensor Real [2]

initState : Tensor Real [2]
initState = [0.0, 0.0]

@property
trajectoryFirst : Bool
trajectoryFirst = (rollout[4] ctrl dyn initState) ! 0 ! 0 >= 0.0
