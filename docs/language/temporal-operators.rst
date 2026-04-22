Temporal operators
==================

.. contents::
   :depth: 1
   :local:

Signal Temporal Logic operators
-------------------------------

Vehicle provides three *Signal Temporal Logic* (STL) operators for
expressing properties over discrete-time boolean signals. Each takes
a bounded interval ``[a, b]`` of step indices and a ``Tensor Bool``
signal, and returns a ``Tensor Bool`` signal of the same shape.
The outer dimension of the signal is the time dimension.

The signal is typically derived from a state trajectory. Most STL
specifications describe *closed-loop* behaviour: a controller and a
dynamics model are rolled out together to produce the trajectory one
step at a time. Temporal operators also apply unchanged to
*open-loop* specifications, where a network predicts the full
trajectory in one forward pass (see
`Open-loop specifications`_ below).

Running example: a closed-loop controller
-----------------------------------------

Suppose you have a controller over a 2-dimensional state
``[position, velocity]`` that outputs an acceleration, and a fixed
dynamics model. Rolling the two out for 10 steps from the origin
produces a ``Tensor Real [10, 2]`` trajectory. Take its first column
to get the sequence of positions, and build an element-wise boolean
signal recording whether the position at each step is within bounds:

.. code-block:: agda

   @network
   controller : Tensor Real [2] -> Tensor Real [1]

   @dynamics
   dynamics : Tensor Real [2] -> Tensor Real [1] -> Tensor Real [2]

   initState : Tensor Real [2]
   initState = [0.0, 0.0]

   trajectory : Tensor Real [10, 2]
   trajectory = rollout[10] controller dynamics initState

   positions : Tensor Real [10]
   positions = (transpose trajectory) ! 0

   belowLimit : Tensor Bool [10]
   belowLimit = const 0 [10] .< positions and positions .< const 10 [10]


Notice the `.<` operator, which is a pointwise comparison that produces a boolean tensor.
The result is a ``Tensor Bool [10]`` signal that holds at step ``t`` when the position at
that step is between 0 and 10. Trying to write it like
``const 0 [10] .< positions .< const 10 [10]`` would not work,
because one comparison would be interpreted as a boolean tensor,
and the other comparison would fail with a type error.


The ``globally`` operator
-------------------------

``globally[a, b] signal`` holds at step ``t`` when the signal
holds at every step in the window ``[t + a, t + b]``. Use it to
specify that a safety property must hold throughout an interval:

.. code-block:: agda

   @property
   stayBounded : Bool
   stayBounded = (globally[0,9] belowLimit) ! 0

The outer ``! 0`` extracts the result at the first time step; because
the window ``[0,9]`` covers the entire trace, this is exactly the
requirement that ``belowLimit`` holds at every step.

The ``finally`` operator
------------------------

``finally[a, b] signal`` holds at step ``t`` when the signal holds
at some step in the window ``[t + a, t + b]``. Use it to specify
that a goal is eventually reached:

.. code-block:: agda

   inGoal : Tensor Bool [10]
   inGoal = foreach i . goalLo <= positions ! i <= goalHi

   @property
   reachGoal : Bool
   reachGoal = (finally[0,9] inGoal) ! 0

The ``until`` operator
----------------------

``until[a, b] phi psi`` holds at step ``t`` when ``psi`` becomes
true at some step ``j`` within ``[t + a, t + b]`` and ``phi`` holds
at every step strictly before ``j``. Use it to combine a progress
requirement with a safety invariant that must hold until the goal
is reached:

.. code-block:: agda

   @property
   safeUntilGoalReached : Bool
   safeUntilGoalReached = (until[0,9] belowLimit inGoal) ! 0

Per-dimension state predicates
------------------------------

The per-step predicate can itself use a first-order quantifier over
the state dimensions. Build the boolean signal with ``foreach`` over
the time dimension and ``forall`` over an ``Index`` type to require
that every component of the state stays within bounds until the
position enters a goal band:

.. code-block:: agda

   stateInBounds : Tensor Real [2] -> Bool
   stateInBounds s = forall k . stateLoBounds ! k <= s ! k <= stateHiBounds ! k

   inGoalRegion : Tensor Real [2] -> Bool
   inGoalRegion s = goalLo <= s ! 0 <= goalHi

   @property
   safeUntilGoal : Bool
   safeUntilGoal = (until[0,9]
                      (foreach t . stateInBounds (trajectory ! t))
                      (foreach t . inGoalRegion (trajectory ! t))) ! 0

The ``forall k`` quantifies over the finite ``Index 2`` type and
expands to a conjunction; the ``foreach t`` builds the boolean
signal at each of the 10 time steps; the ``until[0,9]`` composes the
two signals into a single property.

Make sure to think about how the quantifiers interact with the temporal operators. If used
imporperly, they can lead to expensive computations that take a long time to evaluate.

Open-loop specifications
------------------------

Not every specification needs a dynamics model. An *open-loop*
controller predicts the full trajectory in one forward pass, and the
temporal operators apply to its output directly:

.. code-block:: agda

   @network
   controller : Tensor Real [2] -> Tensor Real [10]

   vTrace : Tensor Real [10]
   vTrace = controller initState

   belowLimit : Tensor Bool [10]
   belowLimit = const 0 [10] .<= vTrace and vTrace .<= const 10 [10]

   @property
   alwaysBelowLimit : Bool
   alwaysBelowLimit = (globally[0,9] belowLimit) ! 0

The operators themselves, and all the patterns above, work the same
way in both styles. The choice is about how the trajectory is
produced, not about which properties can be expressed over it.


Support
------------------------------

Temoporal operators are currently only supported with the loss backend.
