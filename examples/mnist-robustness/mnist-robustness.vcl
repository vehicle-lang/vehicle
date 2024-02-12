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
@network
classifier : Image -> Vector Rat 10

-- The classifier advises that input image `x` has label `i` if the score
-- for label `i` is greater than the score of any other label `j`.
advises : Image -> Label -> Bool
advises x i = forall j . j != i => classifier x ! i > classifier x ! j

--------------------------------------------------------------------------------
-- Definition of robustness around a point

-- First we define the parameter `epsilon` that will represent the radius of the
-- ball that we want the network to be robust in. Note that we declare this as
-- a parameter which allows the value of `epsilon` to be specified at compile
-- time rather than be fixed in the specification.
@parameter
epsilon : Rat

-- Next we define what it means for an image `x` to be in a ball of
-- size epsilon around 0.
boundedByEpsilon : Image -> Bool
boundedByEpsilon x = forall i j . -epsilon <= x ! i ! j <= epsilon

-- We now define what it means for the network to be robust around an image `x`
-- that should be classified as `y`. Namely, that for any perturbation no greater
-- than epsilon then if the perturbed image is still a valid image then the
-- network should still advise label `y` for the perturbed version of `x`.
robustAround : Image -> Label -> Bool
robustAround image label = forall perturbation .
  let perturbedImage = image - perturbation in
  boundedByEpsilon perturbation and validImage perturbedImage =>
    advises perturbedImage label

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
-- the earlier parameter `epsilon`, we set the `infer` option of the
-- parameter `n` to 'True'. This means that it does not need to be provided
--  manually but instead will be automatically inferred by the compiler.
-- In this case it will be inferred from the training datasets.
@parameter(infer=True)
n : Nat

-- We next declare two datasets, the training images and the corresponding
-- training labels. Note that we use the previously declared parameter `n`
-- to enforce that they are the same size.
@dataset
trainingImages : Vector Image n

@dataset
trainingLabels : Vector Label n

-- We then say that the network is robust if it is robust around every pair
-- of input images and output labels. Note the use of the `foreach`
-- keyword when quantifying over the index `i` in the dataset. Whereas `forall`
-- would return a single `Bool`, `foreach` constructs a `Vector` of booleans,
-- ensuring that Vehicle will report on the verification status of each image in
-- the dataset separately. If `forall` was omitted, Vehicle would only
-- report if the network was robust around *every* image in the dataset, a
-- state of affairs which is unlikely to be true.
@property
robust : Vector Bool n
robust = foreach i . robustAround (trainingImages ! i) (trainingLabels ! i)
