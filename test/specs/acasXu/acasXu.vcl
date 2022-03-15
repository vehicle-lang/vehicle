--------------------------------------------------------------------------------
-- Types

pi : Real
pi = 3.141592

--------------------------------------------------------------------------------
-- Constants

type InputVector  = Tensor Real [5]
type OutputVector = Tensor Real [5]

--------------------------------------------------------------------------------
-- Utility functions

minimal : Int -> OutputVector -> Prop
minimal i x =
  x ! 0 >= x ! i and
  x ! 1 >= x ! i and
  x ! 2 >= x ! i and
  x ! 3 >= x ! i and
  x ! 4 >= x ! i and

maximal : Int -> OutputVector -> Prop
maximal i x =
  x ! 0 >= x ! i and
  x ! 1 >= x ! i and
  x ! 2 >= x ! i and
  x ! 3 >= x ! i and
  x ! 4 >= x ! i and

--------------------------------------------------------------------------------
-- The network

network acasXu : InputVector -> OutputVector

--------------------------------------------------------------------------------
-- Inputs

distanceToIntruder : InputVector -> Real
distanceToIntruder x = x ! 0

angleToIntruder : InputVector -> Real
angleToIntruder x = x ! 1

intruderHeading : InputVector -> Real
intruderHeading x = x ! 2

speed : InputVector -> Real
speed x = x ! 3

intruderSpeed : InputVector -> Real
intruderSpeed x = x ! 4

--------------------------------------------------------------------------------
-- Outputs

clearOfConflictScore : InputVector -> Real
clearOfConflictScore x = acasXu x ! 0

weakLeftScore : InputVector -> Real
weakLeftScore x = acasXu x ! 1

weakRightScore : InputVector -> Real
weakRightScore x = acasXu x ! 2

strongLeftScore : InputVector -> Real
strongLeftScore x = acasXu x ! 3

strongRightScore : InputVector -> Real
strongRightScore x = acasXu x ! 4

--------------------------------------------------------------------------------
-- Property 1

-- If the intruder is distant and is significantly slower than the
-- ownship, the score of a COC advisory will always be below a certain fixed
-- threshold.

intruderDistantAndSlower : InputVector -> Prop
intruderDistantAndSlower x =
  distanceToIntruder x >= 55947.691 and
  speed x              >= 1145      and
  intruderSpeed x      <= 60

cocBeneathFixedThreshold : InputVector -> Prop
cocBeneathFixedThreshold x = clearOfConflictScore x <= 1500

property1 : Prop
property1 = forall x . intruderDistantAndSlower x => cocBeneathFixedThreshold x

--------------------------------------------------------------------------------
-- Property 2

-- If the intruder is distant and is significantly slower than the
-- ownship, the score of a COC advisory will never be maximal.

cocMaximal : InputVector -> Prop
cocMaximal = maximal 0

property2 : Prop
property2 = forall x . intruderDistantAndSlower x => not (cocMaximal x)

--------------------------------------------------------------------------------
-- Property 3

-- If the intruder is directly ahead and is moving towards the
-- ownship, the score for COC will not be minimal

directlyAheadAndMovingTowards : InputVector -> Prop
directlyAheadAndMovingTowards x =
  1500  <= distanceToIntruder x <= 1800 and
  −0.06 <= angleToIntruder    x <= 0.06 and
  3.10  <= intruderHeading    x         and
  980   <= speed              x

cocMinimal : InputVector -> Prop
cocMinimal = minimal 0

property3 : Prop
property3 = forall x . directlyAheadAndMovingTowards x => not (cocMinimal x)

--------------------------------------------------------------------------------
-- Property 6

-- If the intruder is sufficiently far away, the network advises COC.

intruderFarAway : InputVector -> Prop
intruderFarAway x =
  (- pi <= angleToIntruder x <= -0.7 or 0.7 <= angleToIntruder x <= pi)
  and 12000 <= distanceToIntruder x <= 62000
  and -pi   <= intruderHeading    x <= -pi + 0.005
  and 100   <= speed              x <= 1200
  and 0     <= intruderSpeed      x <= 1200

property6 : Prop
property6 = forall x . intruderFarAway x => cocMaximal x