--------------------------------------------------------------------------------
-- Temporal safety example
--
-- A controller that maps a 4-dimensional state observation to a 4-dimensional
-- output. We specify three temporal properties over the controller's output
-- signal to express safety, reachability, and reactive response requirements.
--------------------------------------------------------------------------------

-- Type aliases for readability
type State  = Tensor Real [4]
type Signal = Tensor Bool [4]

--------------------------------------------------------------------------------
-- Network declaration

@network
controller : State -> State

-- A fixed input trajectory (e.g., sampled from the environment)
input : State
input = [0.5, -0.3, 0.2, 0.4]

-- The controller's output signal, compared element-wise against zero
output : Signal
output = controller input >. [0, 0, 0, 0]

--------------------------------------------------------------------------------
-- Property 1: Safety (Globally)
--
-- "The controller output remains non-negative at every time step in [0,2]."
--
-- globally[a,b] checks that the property holds at all positions within the
-- sliding window [i+a, i+b] for each starting position i. The ! 0 extracts
-- the result at the first position.

@property
alwaysPositive : Bool
alwaysPositive = forall i . (globally[0,2] output) ! i

--------------------------------------------------------------------------------
-- Property 2: Reachability (Finally)
--
-- "The controller output becomes non-negative at some point within [0,3]."
--
-- finally[a,b] checks that the property holds at least once within the window.

@property
eventuallyPositive : Bool
eventuallyPositive = exists i . (finally[0,3] output) ! i

--------------------------------------------------------------------------------
-- Property 3: Reactive response (Until)
--
-- "A precondition signal holds at every step until a goal signal becomes true,
--  within the interval [0,2]."
--
-- until[a,b] phi psi checks that phi holds at all steps before some step
-- where psi becomes true, and that such a step exists within [a,b].

precondition : Signal
precondition = controller [1.0, 2.0, 3.0, 4.0] >. [0, 0, 0, 0]

goal : Signal
goal = controller [4.0, 3.0, 2.0, 1.0] >. [0, 0, 0, 0]

@property
respondsInTime : Bool
respondsInTime = exists i . (until[0,2] precondition goal) ! i
