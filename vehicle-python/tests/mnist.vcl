--------------------------------------------------------------------------------
-- Inputs and outputs

-- Define the type for our input images
--type Image = Tensor Rat [28, 28]

--------------------------------------------------------------------------------
-- Network

-- Declare the network used to classify images. The output of the network is a
-- score for each of the digits 0 to 9.
@network
mnist : Tensor Rat [28, 28] -> Vector Rat 10

--------------------------------------------------------------------------------
-- Definition of robustness around a point

-- First we define the parameter `epsilon` that will represent the radius of the
-- ball that we want the network to be robust in. Note that we declare this as
-- a parameter which allows the value of `epsilon` to be specified at compile
-- time rather than be fixed in the specification.

--epsilon : Rat
--epsilon = 0.01

--delta : Rat
--delta = 0.02

-- Next we define what it means for an image `x` to be in a ball of
-- size epsilon around 0.
boundedByEpsilon : Tensor Rat [28, 28] -> Bool
boundedByEpsilon x = forall i j . -0.01 <= (x ! i ! j) <= 0.01

--The disjunction avoids having to define absolute value
boundedByDelta : Tensor Rat [28, 28] -> Tensor Rat [28, 28] -> Bool
boundedByDelta x y = forall i . (-0.02 <= (((mnist x) ! i ) - ((mnist y) ! i )) <= 0.02) 


robustSingleInput : Tensor Rat [28, 28] -> Bool
robustSingleInput image = forall pertubation .
    let perturbedImage = image - pertubation in
    boundedByEpsilon pertubation => boundedByDelta image perturbedImage

@property
robust1 : Bool
robust1 = forall x . robustSingleInput x
