@network
controller : Tensor Real [2] -> Tensor Real [1]

@dynamics
dynamics : Tensor Real [2] -> Tensor Real [1] -> Tensor Real [2]

initState : Tensor Real [2]
initState = [0.0, 0.0]

T : Nat
T = 4

trajectory : Tensor Real [T, 2]
trajectory = rollout[T] controller dynamics initState

@property
positionStaysSmall : Bool
positionStaysSmall = forall t . trajectory ! t ! 0 <= 10.0

@property
stateInBounds : Bool
stateInBounds = forall t . forall k .
  -10.0 <= trajectory ! t ! k <= 10.0

@property
boundedInitVel : Bool
boundedInitVel = exists v . 0.0 <= v <= 1.0
                   and (forall t . (rollout[T] controller dynamics [0.0, v]) ! t ! 0 <= 10.0)
