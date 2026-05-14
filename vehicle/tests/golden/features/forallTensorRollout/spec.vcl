-- Regression test for the "Incorrectly sorted slice variables" bug:
-- a `forall (x : Tensor Real [N])` quantifier whose body involves a
-- `rollout[T]` whose controller argument is an inline projection lambda
-- of state -> observation. This combination used to crash the slice-
-- context lookup in `findCorrespondingVariableInOriginalCtx` because
-- the originalLv counter was being computed as a newest-first walk
-- counter rather than the actual binder index in the un-expanded ctx.

import STL

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

@property
test : Bool
test = forall (x : Tensor Real [6]) .
    initialLo < x < initialHi =>
        (rollout T
            (\ s -> controller [s ! 0, s ! 1, s ! 2, s ! 3, s ! 4])
            dynamics
            x) ! 0 ! 0 >= 0.0
