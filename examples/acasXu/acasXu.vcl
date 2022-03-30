--------------------------------------------------------------------------------
-- Full specification of the ACAS XU networks

-- Taken from Appendix VI of "Reluplex: An Efficient SMT Solver for Verifying
-- Deep Neural Networks" at https://arxiv.org/pdf/1702.01135.pdf

-- Comments describing the properties are taken directly from the text.

--------------------------------------------------------------------------------
-- Inputs and outputs

type InputVector = Tensor Rat [5]

distanceToIntruder = 0
angleToIntruder    = 1
intruderHeading    = 2
speed              = 3
intruderSpeed      = 4

type OutputVector = Tensor Rat [5]

clearOfConflict = 0
weakLeft        = 1
-- weakRight    = 2  (not used, so currently type-checker complains)
strongLeft      = 3
strongRight     = 4

--------------------------------------------------------------------------------
-- The network

network acasXu : InputVector -> OutputVector

--------------------------------------------------------------------------------
-- Utilities

pi : Rat
pi = 3.141592

advises : Fin 5 -> Tensor Rat [5] -> Prop
advises i x = forall j . i != j => acasXu x ! i < acasXu x ! j

--------------------------------------------------------------------------------
-- Property 1

-- If the intruder is distant and is significantly slower than the
-- ownship, the score of a COC advisory will always be below a certain fixed
-- threshold.

-- Tested on: all 45 networks.

intruderDistantAndSlower : InputVector -> Prop
intruderDistantAndSlower x =
  x ! distanceToIntruder >= 55947.691 and
  x ! speed              >= 1145      and
  x ! intruderSpeed      <= 60

property1 : Prop
property1 = forall x . intruderDistantAndSlower x =>
  acasXu x ! clearOfConflict <= 1500

--------------------------------------------------------------------------------
-- Property 2

-- If the intruder is distant and is significantly slower than the
-- ownship, the score of a COC advisory will never be maximal.

-- Tested on: N_{x,y} for all x ≥ 2 and for all y

property2 : Prop
property2 = forall x . intruderDistantAndSlower x =>
  (exists j . (acasXu x ! j) > (acasXu x ! clearOfConflict))

--------------------------------------------------------------------------------
-- Property 3

-- If the intruder is directly ahead and is moving towards the
-- ownship, the score for COC will not be minimal.

-- Tested on: all networks except N_{1,7}, N_{1,8}, and N_{1,9}.

directlyAhead : InputVector -> Prop
directlyAhead x =
  1500  <= x ! distanceToIntruder <= 1800 and
  -0.06 <= x ! angleToIntruder    <= 0.06

movingTowards : InputVector -> Prop
movingTowards x =
  x ! intruderHeading >= 3.10  and
  x ! speed           >= 980   and
  x ! intruderSpeed   >= 960

property3 : Prop
property3 = forall x . directlyAhead x and movingTowards x =>
  not (advises clearOfConflict x)

--------------------------------------------------------------------------------
-- Property 4

-- If the intruder is directly ahead and is moving away from the
-- ownship but at a lower speed than that of the ownship, the score for COC
-- will not be minimal.

-- Tested on: all networks except N_{1,7}, N_{1,8}, and N_{1,9}.

movingAway : InputVector -> Prop
movingAway x =
          x ! intruderHeading == 0   and
  1000 <= x ! speed                  and
  700  <= x ! intruderSpeed   <= 800

property4 : Prop
property4 = forall x . directlyAhead x and movingAway x =>
  not (advises clearOfConflict x)

--------------------------------------------------------------------------------
-- Property 5

-- If the intruder is near and approaching from the left, the network
-- advises “strong right”.

-- Tested on: N_{1,1}.

nearAndApproachingFromLeft : InputVector -> Prop
nearAndApproachingFromLeft x =
  250 <= x ! distanceToIntruder <= 400         and
  0.2 <= x ! angleToIntruder    <= 0.4         and
  -pi <= x ! intruderHeading    <= -pi + 0.005 and
  100 <= x ! speed              <= 400         and
  0   <= x ! intruderSpeed      <= 400

property5 : Prop
property5 = forall x . nearAndApproachingFromLeft x => advises strongRight x

--------------------------------------------------------------------------------
-- Property 6

-- If the intruder is sufficiently far away, the network advises COC.

-- Tested on: N_{1,1}.

intruderFarAway : InputVector -> Prop
intruderFarAway x =
  12000 <= x ! distanceToIntruder <= 62000                                  and
  (- pi <= x ! angleToIntruder <= -0.7 or 0.7 <= x ! angleToIntruder <= pi) and
  -pi   <= x ! intruderHeading    <= -pi + 0.005                            and
  100   <= x ! speed              <= 1200                                   and
  0     <= x ! intruderSpeed      <= 1200

property6 : Prop
property6 = forall x . intruderFarAway x => advises clearOfConflict x

--------------------------------------------------------------------------------
-- Property 7

-- If vertical separation is large, the network will never advise a strong turn.

-- Tested on: N_{1,9}.

largeVerticalSeparation : InputVector -> Prop
largeVerticalSeparation x =
  0    <= x ! distanceToIntruder <= 60760  and
  -pi  <= x ! angleToIntruder    <= pi     and
  -pi  <= x ! intruderHeading    <= pi     and
  100  <= x ! speed              <= 1200   and
  0    <= x ! intruderSpeed      <= 1200

property7 : Prop
property7 = forall x . largeVerticalSeparation x =>
  not (advises strongLeft x) and not (advises strongRight x)

--------------------------------------------------------------------------------
-- Property 8

-- For a large vertical separation and a previous “weak left” advisory, the
-- network will either output COC or continue advising “weak left”.

-- Tested on: N_{2,9}.

largeVerticalSeparationAndPreviousWeakLeft : InputVector -> Prop
largeVerticalSeparationAndPreviousWeakLeft x =
  0    <= x ! distanceToIntruder <= 60760    and
  -pi  <= x ! angleToIntruder    <= -0.75*pi and
  -0.1 <= x ! intruderHeading    <= 0.1      and
  600  <= x ! speed              <= 1200     and
  600  <= x ! intruderSpeed      <= 1200

property8 : Prop
property8 = forall x . largeVerticalSeparationAndPreviousWeakLeft x =>
  (advises clearOfConflict x) or (advises weakLeft x)

--------------------------------------------------------------------------------
-- Property 9

-- Even if the previous advisory was “weak right”, the presence of a nearby
-- intruder will cause the network to output a “strong left” advisory instead.

-- Tested on: N_{3,3}.

previousWeakRightAndNearbyIntruder : InputVector -> Prop
previousWeakRightAndNearbyIntruder x =
  2000 <= x ! distanceToIntruder <= 7000       and
  -0.4 <= x ! angleToIntruder    <= -0.14      and
  -pi  <= x ! intruderHeading    <= -pi + 0.01 and
  100  <= x ! speed              <= 150        and
  0    <= x ! intruderSpeed      <= 150

property9 : Prop
property9 = forall x . previousWeakRightAndNearbyIntruder x =>
  advises strongLeft x

--------------------------------------------------------------------------------
-- Property 10

-- For a far away intruder, the network advises COC.

-- Tested on: N_{4,5}.

intruderFarAway2 : InputVector -> Prop
intruderFarAway2 x =
  36000 <= x ! distanceToIntruder <= 60760       and
  0.7   <= x ! angleToIntruder    <= pi          and
  -pi   <= x ! intruderHeading    <= -pi + 0.01  and
  900   <= x ! speed              <= 1200        and
  600   <= x ! intruderSpeed      <= 1200

property10 : Prop
property10 = forall x . intruderFarAway2 x => advises clearOfConflict x
