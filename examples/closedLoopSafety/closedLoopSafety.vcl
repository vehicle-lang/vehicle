--------------------------------------------------------------------------------
-- Double-integrator reach-avoid (canonical STL benchmark)
--
-- State: [position, velocity]
-- Action: [acceleration]
-- Dynamics: x' = x + v*dt, v' = v + u*dt  (dt defined in Python, e.g. 0.4)
--
-- The controller must steer the system from the origin to a goal region
-- [goalLo, goalHi] while keeping position within safe bounds [0, posMax].
--------------------------------------------------------------------------------

import STL

goalLo = 9.0
goalHi = 11.0
posMax = 15.0

-- Tn: tensor dimension horizon (Nat).
-- T: temporal bound horizon (Time) for rollout/operators.
Tn : Nat
Tn = 10

T : Time
T = 10

-- Controller: maps [position, velocity] to [acceleration]
@network
controller : Tensor Real [2] -> Tensor Real [1]

-- Dynamics: maps (state, action) to next state
@dynamics
dynamics : Tensor Real [2] -> Tensor Real [1] -> Tensor Real [2]

-- Start at the origin, at rest
initState : Tensor Real [2]
initState = [0.0, 0.0]

-- Roll out the closed-loop system for 10 steps
trajectory : Tensor Real [Tn, 2]
trajectory = rollout T controller dynamics initState

-- Safety: position stays within [0, posMax] at every step
positions : Tensor Real [Tn]
positions = (transpose trajectory) ! 0

@property
stayBounded : Bool
stayBounded = (globally [0,9]
                (foreach t . 1.0 <= positions ! t <= posMax)) ! 0

-- Liveness: position eventually enters goal region [goalLo, goalHi]
@property
reachGoal : Bool
reachGoal = (finally [0,9]
                (foreach t . goalLo <= positions ! t <= goalHi)) ! 0

-- Per-dimension bounds on the state vector [position, velocity]
stateLoBounds : Tensor Real [2]
stateLoBounds = [0.0, -10.0]

stateHiBounds : Tensor Real [2]
stateHiBounds = [posMax, 10.0]

stateInBounds : Tensor Real [2] -> Bool
stateInBounds s = forall k . stateLoBounds ! k <= s ! k <= stateHiBounds ! k

inGoalRegion : Tensor Real [2] -> Bool
inGoalRegion s = goalLo <= s ! 0 <= goalHi

-- Safety-until-reach: the state stays within valid bounds in every dimension
-- until the position enters the goal band. Mixes `until[0,T - 1]` (temporal) with
-- `forall k` over `Index 2` (first-order over state dimensions).
@property
safeUntilGoal : Bool
safeUntilGoal = (until [0,9]
                   (foreach t . stateInBounds (trajectory ! t))
                   (foreach t . inGoalRegion (trajectory ! t))) ! 0
