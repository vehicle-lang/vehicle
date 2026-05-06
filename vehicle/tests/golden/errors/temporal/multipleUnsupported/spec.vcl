@network
ctrl : Tensor Real [2] -> Tensor Real [1]

@dynamics
dyn : Tensor Real [2] -> Tensor Real [1] -> Tensor Real [2]

initState : Tensor Real [2]
initState = [0.0, 0.0]

T : Time
T = 4

@property
multiFeature : Bool
multiFeature =
  (globally[0, T - 1]
    ((rollout[T] ctrl dyn initState) ! 0 >=. const 0.0 [2])) ! 0
