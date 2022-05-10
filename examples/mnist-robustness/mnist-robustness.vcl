--------------------------------------------------------------------------------
-- Inputs and outputs

-- Define the type for our input images
type Image = Tensor Rat [28, 28]

-- The type of the output labels
-- i.e a number between 0 and 9, one for each digit
type Label = Index 10

-- A predicate that states that all the pixel values in a given image are
-- in the range 0.0 to 1.0
validImage : Image -> Bool
validImage x = forall i j . 0 <= x ! i ! j <= 1

--------------------------------------------------------------------------------
-- Network

-- Declare the network used to classify images. The output of the network is a
-- score for each of the digits 0 to 9.
network mnist : Image -> Tensor Rat [10]

-- The network advises that input image `x` has label `i` if the score
-- for label `i` is less than the score of any other label `j`.
advises : Image -> Label -> Bool
advises x i = forall j . j != i => mnist x ! i < mnist x ! j

--------------------------------------------------------------------------------
-- Definition of robustness around a point

-- First we define the parameter `epsilon` that will represent the radius of the
-- ball that we want the network to be robust in. Note that we declare this as
-- a parameter which allows the value of `epsilon` to be specified at compile
-- time rather than be fixed in the specification.
parameter epsilon : Rat

-- Next we define what it means for an input image `x` to be in an l-infinity
-- ball of a given `radius` around a given `centre` image.
lInfBall : Image -> Image -> Bool
lInfBall centre x = forall i j . -epsilon <= (x - centre) ! i ! j <= epsilon

-- We now define what it means for the network to be robust around an image `x`
-- that should be classified as `y`. Namely, that for any valid input image `z`
-- that lies within the ball of radius `epsilon` around `x` the network should
-- still advise label `y` when asked to classify `z`.
robustAround : Image -> Label -> Bool
robustAround x y = forall z .
  validImage z and lInfBall x z => advises z y

--------------------------------------------------------------------------------
-- Robustness with respect to a dataset

-- We only really care about the network being robust on the set of images it
-- will encounter. Indeed it is much more challenging to expect the network
-- to be robust around all possible images. After all most images will be just
-- be random noise.

-- Unfortunately we can't characterise the set of "reasonable" input images.
-- Instead we approximate it using the training dataset, and ask that the
-- network is robust around images in the training dataset.

-- We first specify parameter `n` the size of the training dataset. Unlike
-- the earlier parameter `epsilon`, `n` is marked as implicit which means
-- that it does not need to be provided manually but instead will be
-- automatically inferred by the compiler. In this case it will be inferred
-- from the training dataset passed.
parameter {n : Nat}

-- We next declare two datasets, the training images and the corresponding
-- training labels. Note that we use the previously declared parameter `n`
-- to enforce that they are the same size.
dataset trainingImages : Tensor Image [n]
dataset trainingLabels : Tensor Label [n]

-- We then say that the network is robust if it is robust around every pair
-- of input images and output labels. Note the use of the `separate`
-- keyword when quantifying over the index `i` in the dataset. This ensures
-- that Vehicle will report on the verification status of each image in
-- the dataset separately. If `separate` was omitted, Vehicle would only
-- report if the network was robust around *every* image in the dataset, a
-- state of affairs which is unlikely to be true.
robust : Bool
robust = forall separate i .
  robustAround (trainingImages ! i) (trainingLabels ! i)
