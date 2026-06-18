
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
  x ! distanceToIntruder >= 55947.691 and
  x ! speed              >= 1145      and
  x ! intruderSpeed      <= 60

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
  1500  <= x ! distanceToIntruder <= 1800 and
  -0.06 <= x ! angleToIntruder    <= 0.06

movingTowards : UnnormalisedInputVector -> Bool
movingTowards x =
  x ! intruderHeading >= 3.10  and
  x ! speed           >= 980   and
  x ! intruderSpeed   >= 960

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
          x ! intruderHeading == 0   and
  1000 <= x ! speed                  and
  700  <= x ! intruderSpeed   <= 800

@property
property4 : Bool
property4 = forall x .
  validInput x and directlyAhead x and movingAway x =>
  not (minimalCoC x)

--------------------------------------------------------------------------------