--------------------------------------------------------------------------------
-- Standard robustness (Exercise #5)

-- One of the robustness definitions catalogued by Casadio et al. Unlike
-- classification robustness, which only asks that the *advised label* is
-- unchanged, standard robustness constrains the network's *output scores*:
--
--   forall x . |x - y| = epsilon  =  |f(x) - f(y)| <= delta
--
-- Note that this mentions the network twice, `f(x)` and `f(y)`, which is why
-- Marabou cannot discharge it.

-- The input for the network is a 28 * 28 image
type Image = Tensor Real [28, 28]

-- The network returns a score for each of the ten digits
type Score = Tensor Real [10]

@network
classifier : Image -> Score

-- All pixels in a valid image have values between 0 and 1
validImage : Image -> Bool
validImage x = forall i j . 0 <= x ! i ! j <= 1

-- The radius of the ball around the original image, in the input space
@parameter
epsilon : Real

-- How far the output scores are allowed to move, in the output space
@parameter
delta : Real

-- |x - y| <= epsilon in the L-infinity norm
closeInputs : Image -> Image -> Bool
closeInputs x y = forall i j . -epsilon <= x ! i ! j - y ! i ! j <= epsilon

-- |f(x) - f(y)| <= delta in the L-infinity norm
closeOutputs : Score -> Score -> Bool
closeOutputs a b = forall i . -delta <= a ! i - b ! i <= delta

-- Standard robustness around a single image y
standardRobustAround : Image -> Bool
standardRobustAround y = forall x .
  validImage x and closeInputs x y =>
    closeOutputs (classifier x) (classifier y)

-- The size of the data set, inferred by the compiler
-- @parameter(infer=True)
-- n : Nat

-- Standard robustness needs only the images: no labels are involved
@dataset
trainingImages : Vector Image 2

@property
standardRobust : Bool
standardRobust = standardRobustAround (trainingImages ! 0)
