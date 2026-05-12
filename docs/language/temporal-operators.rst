Temporal operators
==================

.. note::
   The temporal operators, the ``rollout`` operator and the ``Time`` type
   live in the optional ``STL`` library module. A specification must
   ``import STL`` to use any of them; the ``@dynamics`` annotation is
   available without the import.

.. warning::
   Temporal operators are currently only supported by the loss backend.
   The verifier (Marabou) and ITP (Agda, Rocq, Isabelle, Imandra) backends
   raise an "unsupported in this backend" error if a property uses them.

.. contents::
   :depth: 1
   :local:

Signal Temporal Logic operators
-------------------------------

Vehicle provides three *Signal Temporal Logic* (STL) operators for
discrete-time boolean signals. Each takes a bounded interval as a
``Vector Time 2`` value ``[a, b]`` and a ``Tensor Bool`` signal, and
returns a signal of the same shape. The outer dimension is time. The
interval endpoints must reduce to compile-time constants.

.. list-table::
   :header-rows: 1
   :widths: 20 35 45

   * - Operator
     - Type
     - Meaning at step ``t``
   * - ``globally [a, b] s``
     - ``Vector Time 2 -> Tensor Bool [N] -> Tensor Bool [N]``
     - ``s`` holds at every step in ``[t + a, t + b]``
   * - ``finally [a, b] s``
     - ``Vector Time 2 -> Tensor Bool [N] -> Tensor Bool [N]``
     - ``s`` holds at some step in ``[t + a, t + b]``
   * - ``until [a, b] phi psi``
     - ``Vector Time 2 -> Tensor Bool [N] -> Tensor Bool [N] -> Tensor Bool [N]``
     - ``psi`` holds at some ``j`` in ``[t + a, t + b]`` and ``phi`` holds at every step before ``j``

Signals usually come from a state trajectory. Closed-loop specs roll a
controller and dynamics model step by step. Open-loop specs use a
network to predict the full trajectory in one pass (see
`Open-loop specifications`_ below). The operators are the same.

The ``@dynamics`` annotation
----------------------------

A ``@dynamics``-annotated declaration is a runtime-bound plant model,
analogous to ``@network`` but representing system dynamics:

.. code-block:: agda

   @dynamics
   dynamics : Tensor Real [stateDim] -> Tensor Real [actDim]
           -> Tensor Real [stateDim]

It takes the current state and the controller's action, and returns
the next state. Like ``@network``, the body is supplied at the
Python boundary by ``load_specification``. ``@dynamics`` is available
without ``import STL``.

The ``rollout`` operator
------------------------

``rollout n`` interleaves a state-shaped controller with a
``@dynamics``-shaped plant for ``n`` steps from a given initial state:

.. code-block:: agda

   rollout : (n : Time)
          -> (Tensor Real [stateDim] -> Tensor Real [actDim])
          -> (Tensor Real [stateDim] -> Tensor Real [actDim] -> Tensor Real [stateDim])
          -> Tensor Real [stateDim]
          -> Tensor Real [n, stateDim]

Use ``rollout`` for closed-loop specifications: a small controller
and a separate dynamics model compose into a trajectory. For
open-loop specifications a single network produces the full
trajectory directly, and ``rollout`` is not needed.

Running example: a closed-loop controller
-----------------------------------------

Suppose you have a controller over a 2-dimensional state
``[position, velocity]`` that outputs an acceleration, and a fixed
dynamics model. Rolling the two out for 10 steps from the origin
produces a ``Tensor Real [10, 2]`` trajectory. Take its first column
to get the sequence of positions, and build an element-wise boolean
signal recording whether the position at each step is within bounds:

.. code-block:: agda

   import STL

   @network
   controller : Tensor Real [2] -> Tensor Real [1]

   @dynamics
   dynamics : Tensor Real [2] -> Tensor Real [1] -> Tensor Real [2]

   initState : Tensor Real [2]
   initState = [0.0, 0.0]

   trajectory : Tensor Real [10, 2]
   trajectory = rollout 10 controller dynamics initState

   positions : Tensor Real [10]
   positions = (transpose trajectory) ! 0

   belowLimit : Tensor Bool [10]
   belowLimit = const 0.0 [10] <. positions <. const 10.0 [10]


``<.`` is pointwise less-than. ``lo <. x <. hi`` desugars to
``lo <. x and x <. hi``. The result is a ``Tensor Bool [10]``
signal that is true when the position lies in ``(0, 10)``.


The ``globally`` operator
-------------------------

Given a discrete-time signal ``s``, ``globally [a, b] s`` holds at step
``t`` when ``s`` holds at every step in ``[t + a, t + b]``. Use it to
specify that a safety property must hold throughout an interval:

.. code-block:: agda

   @property
   stayBounded : Bool
   stayBounded = (globally [0,9] belowLimit) ! 0

The outer ``! 0`` extracts the result at the first time step; because
the window ``[0,9]`` covers the entire trace, this is exactly the
requirement that ``belowLimit`` holds at every step.

The ``finally`` operator
------------------------

``finally [a, b] s`` holds at step ``t`` when ``s`` holds at some step
in ``[t + a, t + b]``. Use it to specify that a property is eventually
reached:

.. code-block:: agda

   @property
   eventuallyBelowLimit : Bool
   eventuallyBelowLimit = (finally [0,9] belowLimit) ! 0

Reading at ``t = 0``, this asserts that at some step in the trace
the position is between 0 and 10.

The ``until`` operator
----------------------

``until [a, b] phi psi`` holds at step ``t`` when ``psi`` becomes true
at some ``j`` in ``[t + a, t + b]`` and ``phi`` holds at every step
before ``j``. Use it to combine progress with a safety invariant. With
the running example, suppose we want the position to stay below 10
until it reaches a goal band ``(8, 9)``:

.. code-block:: agda

   inGoal : Tensor Bool [10]
   inGoal = const 8.0 [10] <. positions <. const 9.0 [10]

   @property
   safeUntilGoalReached : Bool
   safeUntilGoalReached = (until [0,9] belowLimit inGoal) ! 0

Per-dimension state predicates
------------------------------

The per-step predicate can itself use a first-order quantifier over
the state dimensions. Build the boolean signal with ``foreach`` over
the time dimension and ``forall`` over an ``Index`` type to require
that every component of the state stays within bounds:

.. code-block:: agda

   stateLoBounds : Tensor Real [2]
   stateLoBounds = [-10.0, -10.0]

   stateHiBounds : Tensor Real [2]
   stateHiBounds = [10.0, 10.0]

   stateInBounds : Tensor Real [2] -> Bool
   stateInBounds s = forall k . stateLoBounds ! k <= s ! k <= stateHiBounds ! k

   @property
   alwaysInBounds : Bool
   alwaysInBounds = (globally [0,9] (foreach t . stateInBounds (trajectory ! t))) ! 0

The ``forall k`` quantifies over the finite ``Index 2`` type and
expands to a conjunction; the ``foreach t`` builds the boolean
signal at each of the 10 time steps; the ``globally [0,9]`` reduces
the signal across the whole trace.

If the only requirement on the state is per-component bounds, the
``forall k`` is interchangeable with a chained pointwise comparison
that returns a ``Tensor Bool [2]`` directly — see the
:doc:`tensors` page for ``<=.`` and friends.

Nested quantifiers can be expensive with temporal operators.

Open-loop specifications
------------------------

Not every specification needs a dynamics model. An *open-loop*
network predicts the full trajectory in one forward pass, and the
temporal operators apply to its output directly:

.. code-block:: agda

   import STL

   @network
   trajectoryPrediction : Tensor Real [2] -> Tensor Real [10]

   vTrace : Tensor Real [10]
   vTrace = trajectoryPrediction initState

   belowLimit : Tensor Bool [10]
   belowLimit = const 0.0 [10] <=. vTrace <=. const 10.0 [10]

   @property
   alwaysBelowLimit : Bool
   alwaysBelowLimit = (globally [0,9] belowLimit) ! 0

The closed-loop version factors the controller from a separate plant
model; the open-loop version learns the composition end-to-end. The
operators themselves, and all the patterns above, work the same way
in both styles. The choice is about how the trajectory is produced,
not about which properties can be expressed over it.
