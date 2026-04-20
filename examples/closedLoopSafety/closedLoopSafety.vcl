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

goalLo = 9.0
goalHi = 11.0
posMax = 15.0

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
trajectory : Tensor Real [10, 2]
trajectory = rollout[10] controller dynamics initState

-- Safety: position stays within [0, posMax] at every step
positions : Tensor Real [10]
positions = (transpose trajectory) ! 0

@property
stayBounded : Bool
stayBounded = (globally[0,9]
                (const 0.0 [10] <. positions and positions <. const posMax [10])) ! 0

-- Liveness: position eventually enters goal region [goalLo, goalHi]
@property
reachGoal : Bool
reachGoal = (finally[0,9]
                (const goalLo [10] <. positions and positions <. const goalHi [10])) ! 0
