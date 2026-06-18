
pi = 3.141592

@tensor 
record InputVector where {
    distanceToIntruder : Real,
    angleToIntruder    : Real,
    intruderHeading    : Real,
    speed              : Real,
    intruderSpeed      : Real
}

@tensor 
record OutputVector where {
    clearOfConflict : Real, 
    weakLeft        : Real, 
    weakRight       : Real, 
    strongLeft      : Real, 
    strongRight     : Real 
}

@network
acasXu : InputVector -> OutputVector


@tensor 
record UnnormalisedInputVector where {
    uDistanceToIntruder : Real,  
    uAngleToIntruder    : Real,   
    uIntruderHeading    : Real,   
    uSpeed              : Real,  
    uIntruderSpeed      : Real   
}


minimumInputValues : UnnormalisedInputVector
minimumInputValues = {
    uDistanceToIntruder = 0.0,
    uAngleToIntruder = -pi,
    uIntruderHeading = -pi,
    uSpeed = 100.0,
    uIntruderSpeed = 0.0
}

maximumInputValues : UnnormalisedInputVector
maximumInputValues = {
    uDistanceToIntruder = 60621.0,
    uAngleToIntruder = pi,
    uIntruderHeading = pi,
    uSpeed = 1200.0,
    uIntruderSpeed = 1200.0
}

validInput : UnnormalisedInputVector -> Bool
validInput x =
  0     <= x.uDistanceToIntruder <= 60261.0 and
  -pi   <= x.uAngleToIntruder    <= pi and
  -pi   <= x.uIntruderHeading    <= pi and
  100.0 <= x.uSpeed              <= 1200.0 and
  0     <= x.uIntruderSpeed      <= 1200.0

meanScalingValues : UnnormalisedInputVector
meanScalingValues = {
    uDistanceToIntruder = 19791.091,
    uAngleToIntruder = 0.0,
    uIntruderHeading = 0.0,
    uSpeed = 650.0,
    uIntruderSpeed = 600.0
}

normalise : UnnormalisedInputVector -> InputVector
normalise x = { distanceToIntruder = (x.uDistanceToIntruder - meanScalingValues.uDistanceToIntruder) / (maximumInputValues.uDistanceToIntruder - minimumInputValues.uDistanceToIntruder),
                angleToIntruder = (x.uAngleToIntruder - meanScalingValues.uAngleToIntruder) / (maximumInputValues.uAngleToIntruder - minimumInputValues.uAngleToIntruder),
                intruderHeading = (x.uIntruderHeading - meanScalingValues.uIntruderHeading ) / (maximumInputValues.uIntruderHeading - minimumInputValues.uIntruderHeading),
                speed = (x.uSpeed - meanScalingValues.uSpeed) / (maximumInputValues.uSpeed - minimumInputValues.uSpeed),
                intruderSpeed = (x.uIntruderSpeed - meanScalingValues.uIntruderSpeed) / (maximumInputValues.uIntruderSpeed - minimumInputValues.uIntruderSpeed)
}

normAcasXu : UnnormalisedInputVector -> OutputVector
normAcasXu x = acasXu (normalise x)


maximalCoC : UnnormalisedInputVector -> Bool
maximalCoC x =
    normAcasXu x.clearOfConflict > (normAcasXu x.weakLeft) and 
    normAcasXu x.clearOfConflict > (normAcasXu x.weakRight) and 
    normAcasXu x.clearOfConflict > (normAcasXu x.strongLeft) and 
    normAcasXu x.clearOfConflict > (normAcasXu x.strongRight)

minimalCoC : UnnormalisedInputVector -> Bool
minimalCoC x =
    normAcasXu x.clearOfConflict < (normAcasXu x.weakLeft) and 
    normAcasXu x.clearOfConflict < (normAcasXu x.weakRight) and 
    normAcasXu x.clearOfConflict < (normAcasXu x.strongLeft) and 
    normAcasXu x.clearOfConflict < (normAcasXu x.strongRight)

-- minimalScore : Index 5 -> UnnormalisedInputVector -> Bool
-- minimalScore i x = forall j . i != j => normAcasXu x ! i < normAcasXu x ! j

minimalStrongRight : UnnormalisedInputVector -> Bool
minimalStrongRight x = 
    normAcasXu x.strongRight < (normAcasXu x.weakLeft) and 
    normAcasXu x.strongRight < (normAcasXu x.weakRight) and 
    normAcasXu x.strongRight < (normAcasXu x.strongLeft) and 
    normAcasXu x.strongRight < (normAcasXu x.clearOfConflict)

minimalStrongLeft : UnnormalisedInputVector -> Bool
minimalStrongLeft x = 
    normAcasXu x.strongLeft < (normAcasXu x.weakLeft) and 
    normAcasXu x.strongLeft < (normAcasXu x.weakRight) and 
    normAcasXu x.strongLeft < (normAcasXu x.strongRight) and 
    normAcasXu x.strongLeft < (normAcasXu x.clearOfConflict)

minimalWeakLeft : UnnormalisedInputVector -> Bool
minimalWeakLeft x = 
    normAcasXu x.weakLeft < (normAcasXu x.strongLeft) and 
    normAcasXu x.weakLeft < (normAcasXu x.weakRight) and 
    normAcasXu x.weakLeft < (normAcasXu x.strongRight) and 
    normAcasXu x.weakLeft < (normAcasXu x.clearOfConflict)

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
  x.uDistanceToIntruder >= 55947.691 and
  x.uSpeed              >= 1145      and
  x.uIntruderSpeed      <= 60

@property
property1 : Bool
property1 = forall x . validInput x and intruderDistantAndSlower x =>
  normAcasXu x.clearOfConflict <= scaleCOCOutput 1500

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
  1500  <= x.uDistanceToIntruder <= 1800 and
  -0.06 <= x.uAngleToIntruder    <= 0.06

movingTowards : UnnormalisedInputVector -> Bool
movingTowards x =
  x.uIntruderHeading >= 3.10  and
  x.uSpeed           >= 980   and
  x.uIntruderSpeed   >= 960

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
          x.uIntruderHeading == 0   and
  1000 <= x.uSpeed                  and
  700  <= x.uIntruderSpeed   <= 800

@property
property4 : Bool
property4 = forall x .
  validInput x and directlyAhead x and movingAway x =>
  not (minimalCoC x)

--------------------------------------------------------------------------------

-- PROPERTIES 5-10 from tensor version for converion

-- Property 5

-- If the intruder is near and approaching from the left, the network
-- advises “strong right”.

-- Tested on: N_{1,1}.

nearAndApproachingFromLeft : UnnormalisedInputVector -> Bool
nearAndApproachingFromLeft x =
  250 <= x.uDistanceToIntruder <= 400         and
  0.2 <= x.uAngleToIntruder    <= 0.4         and
  -pi <= x.uIntruderHeading    <= -pi + 0.005 and
  100 <= x.uSpeed              <= 400         and
  0   <= x.uIntruderSpeed      <= 400

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
  12000 <= x.uDistanceToIntruder <= 62000                                  and
  (- pi <= x.uAngleToIntruder <= -0.7 or 0.7 <= x.uAngleToIntruder <= pi) and
  -pi   <= x.uIntruderHeading    <= -pi + 0.005                            and
  100   <= x.uSpeed              <= 1200                                   and
  0     <= x.uIntruderSpeed      <= 1200

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
  0    <= x.uDistanceToIntruder <= 60760  and
  -pi  <= x.uAngleToIntruder    <= pi     and
  -pi  <= x.uIntruderHeading    <= pi     and
  100  <= x.uSpeed              <= 1200   and
  0    <= x.uIntruderSpeed      <= 1200

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
  0    <= x.uDistanceToIntruder <= 60760    and
  -pi  <= x.uAngleToIntruder    <= -0.75*pi and
  -0.1 <= x.uIntruderHeading    <= 0.1      and
  600  <= x.uSpeed              <= 1200     and
  600  <= x.uIntruderSpeed      <= 1200

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
  2000 <= x.uDistanceToIntruder <= 7000       and
  -0.4 <= x.uAngleToIntruder    <= -0.14      and
  -pi  <= x.uIntruderHeading    <= -pi + 0.01 and
  100  <= x.uSpeed              <= 150        and
  0    <= x.uIntruderSpeed      <= 150

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
  36000 <= x.uDistanceToIntruder <= 60760       and
  0.7   <= x.uAngleToIntruder    <= pi          and
  -pi   <= x.uIntruderHeading    <= -pi + 0.01  and
  900   <= x.uSpeed              <= 1200        and
  600   <= x.uIntruderSpeed      <= 1200

@property
property10 : Bool
property10 = forall x .
  validInput x and intruderFarAway2 x =>
  minimalCoC x