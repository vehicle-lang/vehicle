-- The STL extension: the temporal operators (`globally`/`finally`/`until`),
-- closed-loop `rollout`, and the `Time` type. Not auto-imported; `import STL`.

import Definitions

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

type Time = stlTime

@instance(default=2)
timeHasAdd : HasAdd Time Time Time
timeHasAdd = { addTC = addTime }

@instance(default=1)
timeHasSub : HasSub Time Time Time
timeHasSub = { subTC = subTime }

@instance(default=2)
timeHasMul : HasMul Time Time Time
timeHasMul = { mulTC = mulTime }

@instance(default=1)
timeHasDiv : HasDiv Time Time Time
timeHasDiv = { divTC = divTime }

--------------------------------------------------------------------------------
-- Temporal operators
--------------------------------------------------------------------------------
-- The interval is a `Vector Time 2` value: `globally [lo, hi] signal`.

globally : Vector Time 2 -> Tensor Bool dims -> Tensor Bool dims
globally interval signal = stlGlobally (interval ! 0) (interval ! 1) signal

finally : Vector Time 2 -> Tensor Bool dims -> Tensor Bool dims
finally interval signal = stlFinally (interval ! 0) (interval ! 1) signal

until : Vector Time 2 -> Tensor Bool dims -> Tensor Bool dims -> Tensor Bool dims
until interval signal1 signal2 = stlUntil (interval ! 0) (interval ! 1) signal1 signal2

--------------------------------------------------------------------------------
-- Closed-loop rollout
--------------------------------------------------------------------------------
-- Unroll the closed loop for `n` steps: `controller` picks each action,
-- `dynamics` advances the state.

rollout n controller dynamics initialState = stlRollout n controller dynamics initialState
