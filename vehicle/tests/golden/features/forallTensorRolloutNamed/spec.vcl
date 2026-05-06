-- Regression test for the "Incorrectly sorted slice variables" bug,
-- variant where the rollout's controller wrapper is a *named* function
-- rather than an inline lambda. Both forms get inlined during
-- normalisation, so both must compile under the slice-context fix.

T : Time
T = 3

@network
controller : Tensor Real [5] -> Tensor Real [1]

@dynamics
dynamics : Tensor Real [6] -> Tensor Real [1] -> Tensor Real [6]

initialLo : Tensor Real [6]
initialLo = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

initialHi : Tensor Real [6]
initialHi = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]

stateToObs : Tensor Real [6] -> Tensor Real [5]
stateToObs s = [s ! 0, s ! 1, s ! 2, s ! 3, s ! 4]

controllerOnState : Tensor Real [6] -> Tensor Real [1]
controllerOnState s = controller (stateToObs s)

@property
test : Bool
test = forall (x : Tensor Real [6]) .
    initialLo < x < initialHi =>
        (rollout[T] controllerOnState dynamics x) ! 0 ! 0 >= 0.0
