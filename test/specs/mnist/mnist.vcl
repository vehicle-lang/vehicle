--------------------------------------------------------------------------------
-- Input images

-- Define the type of our input images

type Image = Tensor Rat [784]

-- A predicate to ensure that all the pixel values in the image are in the
-- range 0.0 to 1.0

validImage : Image -> Bool
validImage x = forall i . 0 <= x ! i <= 1

--------------------------------------------------------------------------------
-- Network

network mnist : Image -> Tensor Rat [10]

epsilon = 0.1
delta   = 0.1

lInfBall : forall n . Rat -> Tensor Rat [n] -> Tensor Rat [n] -> Bool
lInfBall radius centre x = forall i . -radius <= (x - centre) ! i <= radius

robustAround : Image -> Bool
robustAround x = forall z .
  validImage z and lInfBall epsilon x z =>
  lInfBall delta (mnist x) (mnist z)

--------------------------------------------------------------------------------
-- Robustness with respect to a dataset

-- We then ask for the dataset

dataset trainingImages : Tensor Image [n]
dataset trainingLabels : Tensor (List (Fin 10)) [n]

robustness : Bool
robustness = forall seperate i . robustAround (trainingData ! i) (trainingLabels ! i)