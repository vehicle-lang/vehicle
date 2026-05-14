import STL

@network
controller : Tensor Real [1] -> Tensor Real [1]

@dynamics
dynamics : Tensor Real [1] -> Tensor Real [1] -> Tensor Real [1]

initState : Tensor Real [1]
initState = [0.0]

trajectory : Tensor Real [4, 1]
trajectory = rollout 4 controller dynamics initState

positions : Tensor Real [4]
positions = (transpose trajectory) ! 0

@property
bounded : Bool
bounded = (globally [0,3] (foreach t . -1.0 < positions ! t < 1.0)) ! 0
