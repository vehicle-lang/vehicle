
pi = 3.141592

type InputVector = Tensor Real [5]

distanceToIntruder = 0   -- measured in metres
angleToIntruder    = 1   -- measured in radians
intruderHeading    = 2   -- measured in radians
speed              = 3   -- measured in metres/second
intruderSpeed      = 4   -- measured in meters/second

type OutputVector = Tensor Real [5]

clearOfConflict = 0
weakLeft        = 1
weakRight       = 2
strongLeft      = 3
strongRight     = 4

@network
acasXu : InputVector -> OutputVector


type UnnormalisedInputVector = Tensor Real [5]
uDistanceToIntruder = 0
uAngleToIntruder    = 1   -- measured in radians
uIntruderHeading    = 2   -- measured in radians
uSpeed              = 3   -- measured in metres/second
uIntruderSpeed      = 4   -- measured in meters/second


minimumInputValues : UnnormalisedInputVector
minimumInputValues = [0.0, -pi, -pi, 100.0, 0.0]

maximumInputValues : UnnormalisedInputVector
maximumInputValues = [60261.0, pi, pi, 1200.0, 1200.0]


validInput : UnnormalisedInputVector -> Bool
validInput x =
  0     <= x ! distanceToIntruder <= 60261.0 and
  -pi   <= x ! angleToIntruder    <= pi and
  -pi   <= x ! intruderHeading    <= pi and
  100.0 <= x ! speed              <= 1200.0 and
  0     <= x ! intruderSpeed      <= 1200.0



-- Then the mean values that will be used to scale the inputs.
meanScalingValues : UnnormalisedInputVector
meanScalingValues = [19791.091, 0.0, 0.0, 650.0, 600.0]

-- We can now define the normalisation function that takes an input vector and
-- returns the unnormalised version.
normalise : UnnormalisedInputVector -> InputVector
normalise x = [ (x ! 0 - meanScalingValues ! 0) / (maximumInputValues ! 0 - minimumInputValues ! 0), (x ! 1 - meanScalingValues ! 1) / (maximumInputValues ! 1 - minimumInputValues ! 1), (x ! 2 - meanScalingValues ! 2) / (maximumInputValues ! 2 - minimumInputValues ! 2), (x ! 3 - meanScalingValues ! 3) / (maximumInputValues ! 3 - minimumInputValues ! 3), (x ! 4 - meanScalingValues ! 4) / (maximumInputValues ! 4 - minimumInputValues ! 4) ]

normAcasXu : UnnormalisedInputVector -> OutputVector
normAcasXu x = acasXu (normalise x)


maximalCoC : UnnormalisedInputVector -> Bool
maximalCoC x =
    normAcasXu x ! clearOfConflict > (normAcasXu x ! 1) and 
    normAcasXu x ! clearOfConflict > (normAcasXu x ! 2) and 
    normAcasXu x ! clearOfConflict > (normAcasXu x ! 3) and 
    normAcasXu x ! clearOfConflict > (normAcasXu x ! 4)

minimalCoC : UnnormalisedInputVector -> Bool
minimalCoC x =
    normAcasXu x ! clearOfConflict < (normAcasXu x ! 1) and 
    normAcasXu x ! clearOfConflict < (normAcasXu x ! 2) and 
    normAcasXu x ! clearOfConflict < (normAcasXu x ! 3) and 
    normAcasXu x ! clearOfConflict < (normAcasXu x ! 4)


minimalStrongRight : UnnormalisedInputVector -> Bool
minimalStrongRight x = 
    normAcasXu x ! strongRight < (normAcasXu x ! clearOfConflict) and
    normAcasXu x ! strongRight < (normAcasXu x ! weakLeft) and 
    normAcasXu x ! strongRight < (normAcasXu x ! weakRight) and 
    normAcasXu x ! strongRight < (normAcasXu x ! strongLeft)

minimalStrongLeft : UnnormalisedInputVector -> Bool
minimalStrongLeft x = 
    normAcasXu x ! strongLeft < (normAcasXu x ! clearOfConflict) and
    normAcasXu x ! strongLeft < (normAcasXu x ! weakLeft) and
    normAcasXu x ! strongLeft < (normAcasXu x ! weakRight) and 
    normAcasXu x ! strongLeft < (normAcasXu x ! strongRight)

minimalWeakLeft : UnnormalisedInputVector -> Bool
minimalWeakLeft x = 
    normAcasXu x ! weakLeft < (normAcasXu x ! clearOfConflict) and
    normAcasXu x ! weakLeft < (normAcasXu x ! weakRight) and 
    normAcasXu x ! weakLeft < (normAcasXu x ! strongLeft) and 
    normAcasXu x ! weakLeft < (normAcasXu x ! strongRight)

--------------------------------------------------------------------------------
-- Property 1

-- If the intruder is distant and is significantly slower than the
-- ownship, the score of a COC advisory will always be below a certain fixed
-- threshold.

-- Tested on: all 45 networks.

scaleCOCOutput : Real -> Real
scaleCOCOutput x = (x - 7.518884) / 373.94992

intruderDistantAndSlower : UnnormalisedInputVector -> Bool
intruderDistantAndSlower x =
  x ! uDistanceToIntruder >= 55947.691 and
  x ! uSpeed              >= 1145      and
  x ! uIntruderSpeed      <= 60

@property
property1 : Bool
property1 = forall x . validInput x and intruderDistantAndSlower x =>
  normAcasXu x ! clearOfConflict <= scaleCOCOutput 1500

--------------------------------------------------------------------------------
-- Property 2

-- If the intruder is distant and is significantly slower than the
-- ownship, the score of a COC advisory will never be maximal.

-- Tested on: N_{x,y} for all x ≥ 2 and for all y

@property
property2 : Bool
property2 = forall x .
  validInput x and intruderDistantAndSlower x =>
  not (maximalCoC x)

--------------------------------------------------------------------------------
-- Property 3

-- If the intruder is directly ahead and is moving towards the
-- ownship, the score for COC will not be minimal.

-- Tested on: all networks except N_{1,7}, N_{1,8}, and N_{1,9}.

directlyAhead : UnnormalisedInputVector -> Bool
directlyAhead x =
  1500  <= x ! uDistanceToIntruder <= 1800 and
  -0.06 <= x ! uAngleToIntruder    <= 0.06

movingTowards : UnnormalisedInputVector -> Bool
movingTowards x =
  x ! uIntruderHeading >= 3.10  and
  x ! uSpeed           >= 980   and
  x ! uIntruderSpeed   >= 960

@property
property3 : Bool
property3 = forall x .
  validInput x and directlyAhead x and movingTowards x =>
  not (minimalCoC x)

--------------------------------------------------------------------------------
-- Property 4

-- If the intruder is directly ahead and is moving away from the
-- ownship but at a lower speed than that of the ownship, the score for COC
-- will not be minimal.

-- Tested on: all networks except N_{1,7}, N_{1,8}, and N_{1,9}.

movingAway : UnnormalisedInputVector -> Bool
movingAway x =
          x ! uIntruderHeading == 0   and
  1000 <= x ! uSpeed                  and
  700  <= x ! uIntruderSpeed   <= 800

@property
property4 : Bool
property4 = forall x .
  validInput x and directlyAhead x and movingAway x =>
  not (minimalCoC x)

--------------------------------------------------------------------------------

-- Property 5

-- If the intruder is near and approaching from the left, the network
-- advises “strong right”.

-- Tested on: N_{1,1}.

nearAndApproachingFromLeft : UnnormalisedInputVector -> Bool
nearAndApproachingFromLeft x =
  250 <= x ! uDistanceToIntruder <= 400         and
  0.2 <= x ! uAngleToIntruder    <= 0.4         and
  -pi <= x ! uIntruderHeading    <= -pi + 0.005 and
  100 <= x ! uSpeed              <= 400         and
  0   <= x ! uIntruderSpeed      <= 400

@property
property5 : Bool
property5 = forall x .
  validInput x and nearAndApproachingFromLeft x =>
  minimalStrongRight x

--------------------------------------------------------------------------------
-- Property 6

-- If the intruder is sufficiently far away, the network advises COC.

-- Tested on: N_{1,1}.

intruderFarAway : UnnormalisedInputVector -> Bool
intruderFarAway x =
  12000 <= x ! uDistanceToIntruder <= 62000                                  and
  (- pi <= x ! uAngleToIntruder <= -0.7 or 0.7 <= x ! uAngleToIntruder <= pi) and
  -pi   <= x ! uIntruderHeading    <= -pi + 0.005                            and
  100   <= x ! uSpeed              <= 1200                                   and
  0     <= x ! uIntruderSpeed      <= 1200

@property
property6 : Bool
property6 = forall x .
  validInput x and intruderFarAway x =>
  minimalCoC x

--------------------------------------------------------------------------------
-- Property 7

-- If vertical separation is large, the network will never advise a strong turn.

-- Tested on: N_{1,9}.

largeVerticalSeparation : UnnormalisedInputVector -> Bool
largeVerticalSeparation x =
  0    <= x ! uDistanceToIntruder <= 60760  and
  -pi  <= x ! uAngleToIntruder    <= pi     and
  -pi  <= x ! uIntruderHeading    <= pi     and
  100  <= x ! uSpeed              <= 1200   and
  0    <= x ! uIntruderSpeed      <= 1200

@property
property7 : Bool
property7 = forall x .
  validInput x and largeVerticalSeparation x =>
  not (minimalStrongLeft x) and not (minimalStrongRight x)

--------------------------------------------------------------------------------
-- Property 8

-- For a large vertical separation and a previous “weak left” advisory, the
-- network will either output COC or continue advising “weak left”.

-- Tested on: N_{2,9}.

largeVerticalSeparationAndPreviousWeakLeft : UnnormalisedInputVector -> Bool
largeVerticalSeparationAndPreviousWeakLeft x =
  0    <= x ! uDistanceToIntruder <= 60760    and
  -pi  <= x ! uAngleToIntruder    <= -0.75*pi and
  -0.1 <= x ! uIntruderHeading    <= 0.1      and
  600  <= x ! uSpeed              <= 1200     and
  600  <= x ! uIntruderSpeed      <= 1200

@property
property8 : Bool
property8 = forall x .
  validInput x and largeVerticalSeparationAndPreviousWeakLeft x =>
  (minimalCoC x) or (minimalWeakLeft x)

--------------------------------------------------------------------------------
-- Property 9

-- Even if the previous advisory was “weak right”, the presence of a nearby
-- intruder will cause the network to output a “strong left” advisory instead.

-- Tested on: N_{3,3}.

previousWeakRightAndNearbyIntruder : UnnormalisedInputVector -> Bool
previousWeakRightAndNearbyIntruder x =
  2000 <= x ! uDistanceToIntruder <= 7000       and
  -0.4 <= x ! uAngleToIntruder    <= -0.14      and
  -pi  <= x ! uIntruderHeading    <= -pi + 0.01 and
  100  <= x ! uSpeed              <= 150        and
  0    <= x ! uIntruderSpeed      <= 150

@property
property9 : Bool
property9 = forall x .
  validInput x and previousWeakRightAndNearbyIntruder x =>
  minimalStrongLeft x

--------------------------------------------------------------------------------
-- Property 10

-- For a far away intruder, the network advises COC.

-- Tested on: N_{4,5}.

intruderFarAway2 : UnnormalisedInputVector -> Bool
intruderFarAway2 x =
  36000 <= x ! uDistanceToIntruder <= 60760       and
  0.7   <= x ! uAngleToIntruder    <= pi          and
  -pi   <= x ! uIntruderHeading    <= -pi + 0.01  and
  900   <= x ! uSpeed              <= 1200        and
  600   <= x ! uIntruderSpeed      <= 1200

@property
property10 : Bool
property10 = forall x .
  validInput x and intruderFarAway2 x =>
  minimalCoC x