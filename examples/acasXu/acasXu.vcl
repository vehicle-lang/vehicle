--------------------------------------------------------------------------------
-- Full specification of the ACAS XU networks

-- Taken from Appendix VI of "Reluplex: An Efficient SMT Solver for Verifying
-- Deep Neural Networks" at https://arxiv.org/pdf/1702.01135.pdf

-- Comments describing the properties are taken directly from the text.

--------------------------------------------------------------------------------
-- Utilities

-- The value of the constant `pi`.
pi = 3.141592

--------------------------------------------------------------------------------
-- Inputs

-- We first define a new name for the type of inputs of the network.
-- In particular, it takes inputs of the form of a vector of 5 rational numbers.

type InputVector = Vector Rat 5

-- Next we add meaningful names for the indices.
-- The fact that all vector types come annotated with their size means that it
-- is impossible to mess up indexing into vectors, e.g. if you changed
-- `distanceToIntruder = 0` to `distanceToIntruder = 5` the specification would
-- fail to type-check.

distanceToIntruder = 0   -- measured in metres
angleToIntruder    = 1   -- measured in radians
intruderHeading    = 2   -- measured in radians
speed              = 3   -- measured in metres/second
intruderSpeed      = 4   -- measured in meters/second

--------------------------------------------------------------------------------
-- Outputs

-- Outputs are also a vector of 5 rationals. Each one representing the score
-- for the 5 available courses of action.

type OutputVector = Vector Rat 5

-- Again we define meaningful names for the indices into output vectors.

clearOfConflict = 0
weakLeft        = 1
weakRight       = 2
strongLeft      = 3
strongRight     = 4

--------------------------------------------------------------------------------
-- The network

-- Next we use the `network` annotation to declare the name and the type of the
-- neural network we are verifying. The implementation is passed to the compiler
-- via a reference to the ONNX file at compile time.

@network
acasXu : InputVector -> OutputVector

--------------------------------------------------------------------------------
-- Normalisation

-- As is common in machine learning, the network operates over
-- normalised values, rather than values in the problem space
-- (e.g. using standard units like m/s).
-- This is an issue for us, as we would like to write our specification in
-- terms of the problem space values .
-- Therefore before applying the network, we first have to normalise
-- the values in the problem space.

-- For clarity, we therefore define a new type synonym
-- for unnormalised input vectors which are in the problem space.
type UnnormalisedInputVector = Vector Rat 5

-- Next we define the minimum and maximum values that each input can take.
-- These correspond to the range of the inputs that the network is designed
-- to work over.
minimumInputValues : UnnormalisedInputVector
minimumInputValues = [0.0, -pi, -pi, 100.0, 0.0]

maximumInputValues : UnnormalisedInputVector
maximumInputValues = [60261.0, pi, pi, 1200.0, 1200.0]

-- We can therefore define a simple predicate saying whether a given input
-- vector is in the right range.
validInput : UnnormalisedInputVector -> Bool
validInput x = forall i . minimumInputValues ! i <= x ! i <= maximumInputValues ! i

-- Then the mean values that will be used to scale the inputs.
meanScalingValues : UnnormalisedInputVector
meanScalingValues = [19791.091, 0.0, 0.0, 650.0, 600.0]

-- We can now define the normalisation function that takes an input vector and
-- returns the unnormalised version.
normalise : UnnormalisedInputVector -> InputVector
normalise x = foreach i .
  (x ! i - meanScalingValues ! i) / (maximumInputValues ! i - minimumInputValues ! i)

-- Using this we can define a new function that first normalises the input
-- vector and then applies the neural network.
normAcasXu : UnnormalisedInputVector -> OutputVector
normAcasXu x = acasXu (normalise x)

-- A constraint that says the network chooses output `i` when given the
-- input `x`. The output with the minimal score is chosen.
-- We must necessarily provide a finite index that is less than 5
-- (i.e. of type Index 5). The `a ! b` operator lookups index `b` in vector `a`.
minimalScore : Index 5 -> UnnormalisedInputVector -> Bool
minimalScore i x = forall j . i != j => normAcasXu x ! i < normAcasXu x ! j

maximalScore : Index 5 -> UnnormalisedInputVector -> Bool
maximalScore i x = forall j . i != j => normAcasXu x ! i > normAcasXu x ! j

--------------------------------------------------------------------------------
-- Property 1

-- If the intruder is distant and is significantly slower than the
-- ownship, the score of a COC advisory will always be below a certain fixed
-- threshold.

-- Tested on: all 45 networks.

scaleCOCOutput : Rat -> Rat
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
  not (maximalScore clearOfConflict x)

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
  not (minimalScore clearOfConflict x)

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
  not (minimalScore clearOfConflict x)

--------------------------------------------------------------------------------
-- Property 5

-- If the intruder is near and approaching from the left, the network
-- advises “strong right”.

-- Tested on: N_{1,1}.

nearAndApproachingFromLeft : UnnormalisedInputVector -> Bool
nearAndApproachingFromLeft x =
  250 <= x ! distanceToIntruder <= 400         and
  0.2 <= x ! angleToIntruder    <= 0.4         and
  -pi <= x ! intruderHeading    <= -pi + 0.005 and
  100 <= x ! speed              <= 400         and
  0   <= x ! intruderSpeed      <= 400

@property
property5 : Bool
property5 = forall x .
  validInput x and nearAndApproachingFromLeft x =>
  minimalScore strongRight x

--------------------------------------------------------------------------------
-- Property 6

-- If the intruder is sufficiently far away, the network advises COC.

-- Tested on: N_{1,1}.

intruderFarAway : UnnormalisedInputVector -> Bool
intruderFarAway x =
  12000 <= x ! distanceToIntruder <= 62000                                  and
  (- pi <= x ! angleToIntruder <= -0.7 or 0.7 <= x ! angleToIntruder <= pi) and
  -pi   <= x ! intruderHeading    <= -pi + 0.005                            and
  100   <= x ! speed              <= 1200                                   and
  0     <= x ! intruderSpeed      <= 1200

@property
property6 : Bool
property6 = forall x .
  validInput x and intruderFarAway x =>
  minimalScore clearOfConflict x

--------------------------------------------------------------------------------
-- Property 7

-- If vertical separation is large, the network will never advise a strong turn.

-- Tested on: N_{1,9}.

largeVerticalSeparation : UnnormalisedInputVector -> Bool
largeVerticalSeparation x =
  0    <= x ! distanceToIntruder <= 60760  and
  -pi  <= x ! angleToIntruder    <= pi     and
  -pi  <= x ! intruderHeading    <= pi     and
  100  <= x ! speed              <= 1200   and
  0    <= x ! intruderSpeed      <= 1200

@property
property7 : Bool
property7 = forall x .
  validInput x and largeVerticalSeparation x =>
  not (minimalScore strongLeft x) and not (minimalScore strongRight x)

--------------------------------------------------------------------------------
-- Property 8

-- For a large vertical separation and a previous “weak left” advisory, the
-- network will either output COC or continue advising “weak left”.

-- Tested on: N_{2,9}.

largeVerticalSeparationAndPreviousWeakLeft : UnnormalisedInputVector -> Bool
largeVerticalSeparationAndPreviousWeakLeft x =
  0    <= x ! distanceToIntruder <= 60760    and
  -pi  <= x ! angleToIntruder    <= -0.75*pi and
  -0.1 <= x ! intruderHeading    <= 0.1      and
  600  <= x ! speed              <= 1200     and
  600  <= x ! intruderSpeed      <= 1200

@property
property8 : Bool
property8 = forall x .
  validInput x and largeVerticalSeparationAndPreviousWeakLeft x =>
  (minimalScore clearOfConflict x) or (minimalScore weakLeft x)

--------------------------------------------------------------------------------
-- Property 9

-- Even if the previous advisory was “weak right”, the presence of a nearby
-- intruder will cause the network to output a “strong left” advisory instead.

-- Tested on: N_{3,3}.

previousWeakRightAndNearbyIntruder : UnnormalisedInputVector -> Bool
previousWeakRightAndNearbyIntruder x =
  2000 <= x ! distanceToIntruder <= 7000       and
  -0.4 <= x ! angleToIntruder    <= -0.14      and
  -pi  <= x ! intruderHeading    <= -pi + 0.01 and
  100  <= x ! speed              <= 150        and
  0    <= x ! intruderSpeed      <= 150

@property
property9 : Bool
property9 = forall x .
  validInput x and previousWeakRightAndNearbyIntruder x =>
  minimalScore strongLeft x

--------------------------------------------------------------------------------
-- Property 10

-- For a far away intruder, the network advises COC.

-- Tested on: N_{4,5}.

intruderFarAway2 : UnnormalisedInputVector -> Bool
intruderFarAway2 x =
  36000 <= x ! distanceToIntruder <= 60760       and
  0.7   <= x ! angleToIntruder    <= pi          and
  -pi   <= x ! intruderHeading    <= -pi + 0.01  and
  900   <= x ! speed              <= 1200        and
  600   <= x ! intruderSpeed      <= 1200

@property
property10 : Bool
property10 = forall x .
  validInput x and intruderFarAway2 x =>
  minimalScore clearOfConflict x
