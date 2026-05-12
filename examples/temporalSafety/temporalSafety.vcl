--------------------------------------------------------------------------------
-- Adaptive Cruise Control (ACC) temporal safety specification
--
-- A controller receives an initial state (the following vehicle's speed and
-- relative gap to the lead vehicle) and outputs a predicted velocity
-- trajectory over T=10 time steps. Variables retain the ARCH-COMP "ego"
-- naming for cross-reference with the published benchmark.
-- Three temporal properties are verified over that trajectory.
--
--   alwaysBelowLimit   (globally)   speed never exceeds the hard limit
--   eventuallyAtTarget (finally)    speed reaches the cruise target at some step
--   limitUntilTarget   (until)      speed stays within limit until cruise reached
--------------------------------------------------------------------------------

import STL

-- Physical constants (types inferred from usage context)
vTarget  = 30.0   -- target cruising speed (m/s)
vMax     = 33.0   -- hard speed limit (m/s)
epsilon  = 1.5    -- acceptable deviation from target speed (m/s)
maxAccel = 5.0    -- maximum speed change between consecutive steps (m/s)

T : Nat
T = 10

--------------------------------------------------------------------------------
-- Network declaration
-- Input:  (v_ego, d_rel) — ego speed (m/s) and gap to lead vehicle (m)
-- Output: predicted velocity trajectory over T time steps

@network
controller : Tensor Real [2] -> Tensor Real [T]

-- Fixed initial state: ego at 15 m/s, 30 m behind the lead vehicle
initState : Tensor Real [2]
initState = [15.0, 30.0]

-- Predicted velocity trace produced by the controller
vTrace : Tensor Real [T]
vTrace = controller initState

--------------------------------------------------------------------------------
-- Element-wise boolean signals over the trajectory

-- True at each step where speed is within physical limits [0, vMax]
belowLimit : Tensor Bool [T]
belowLimit = foreach i . 0 <= vTrace ! i <= vMax

-- True at each step where speed is within epsilon of the target
atTarget : Tensor Bool [T]
atTarget = foreach i . vTarget - epsilon <= vTrace ! i <= vTarget + epsilon

--------------------------------------------------------------------------------
-- Temporal properties

-- Safety: speed must stay within physical limits [0, vMax] at every step,
-- and no consecutive step change may exceed maxAccel (law of physics).
-- VCL's type system does not support i+1 index arithmetic in foreach, so the
-- nine step-to-step smoothness checks are enumerated explicitly.
@property
alwaysBelowLimit : Bool
alwaysBelowLimit = ((globally [0,9] belowLimit) ! 0)
  and -maxAccel <= vTrace ! 1 - vTrace ! 0 <= maxAccel
  and -maxAccel <= vTrace ! 2 - vTrace ! 1 <= maxAccel
  and -maxAccel <= vTrace ! 3 - vTrace ! 2 <= maxAccel
  and -maxAccel <= vTrace ! 4 - vTrace ! 3 <= maxAccel
  and -maxAccel <= vTrace ! 5 - vTrace ! 4 <= maxAccel
  and -maxAccel <= vTrace ! 6 - vTrace ! 5 <= maxAccel
  and -maxAccel <= vTrace ! 7 - vTrace ! 6 <= maxAccel
  and -maxAccel <= vTrace ! 8 - vTrace ! 7 <= maxAccel
  and -maxAccel <= vTrace ! 9 - vTrace ! 8 <= maxAccel

-- Reachability: speed must reach the target band at some point
@property
eventuallyAtTarget : Bool
eventuallyAtTarget = (finally [0,9] atTarget) ! 0

-- Progress: speed stays within the hard limit until the target is reached
@property
limitUntilTarget : Bool
limitUntilTarget = (until [0,9] belowLimit atTarget) ! 0
