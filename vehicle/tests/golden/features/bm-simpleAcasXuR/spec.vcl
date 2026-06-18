
pi = 3.141592

@tensor 
record InputVector where {
    distanceToIntruder : Real,   -- measured in metres
    angleToIntruder    : Real,   -- measured in radians
    intruderHeading    : Real,   -- measured in radians
    speed              : Real,   -- measured in metres/second
    intruderSpeed      : Real    -- measured in meters/second
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

-- [0.0, -pi, -pi, 100.0, 0.0]

maximumInputValues : UnnormalisedInputVector
maximumInputValues = {
    uDistanceToIntruder = 60621.0,
    uAngleToIntruder = pi,
    uIntruderHeading = pi,
    uSpeed = 1200.0,
    uIntruderSpeed = 1200.0
}

--  [60261.0, pi, pi, 1200.0, 1200.0]


validInput : UnnormalisedInputVector -> Bool
validInput x =
  0     <= x.uDistanceToIntruder <= 60261.0 and
  -pi   <= x.uAngleToIntruder    <= pi and
  -pi   <= x.uIntruderHeading    <= pi and
  100.0 <= x.uSpeed              <= 1200.0 and
  0     <= x.uIntruderSpeed      <= 1200.0



-- Then the mean values that will be used to scale the inputs.
-- meanScalingValues : UnnormalisedInputVector
-- meanScalingValues = [19791.091, 0.0, 0.0, 650.0, 600.0]

meanScalingValues : UnnormalisedInputVector
meanScalingValues = {
    uDistanceToIntruder = 19791.091,
    uAngleToIntruder = 0.0,
    uIntruderHeading = 0.0,
    uSpeed = 650.0,
    uIntruderSpeed = 600.0
}

-- We can now define the normalisation function that takes an input vector and
-- returns the unnormalised version.
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