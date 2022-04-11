--------------------------------------------------------------------------------
-- Input images

-- Define the type of our input images

type Image = Tensor Rat [784]
type Label = Fin 10

-- A predicate to ensure that all the pixel values in the image are in the
-- range 0.0 to 1.0

validImage : Image -> Bool
validImage x = forall i . 0 <= x ! i <= 1

--------------------------------------------------------------------------------
-- Network

network mnist : Image -> Tensor Rat [10]

parameter epsilon : Nat
parameter delta   : Nat

lInfBall : forall n . Rat -> Tensor Rat [n] -> Tensor Rat [n] -> Bool
lInfBall radius centre x = forall i . -radius <= (x - centre) ! i <= radius

robustAround : Image -> Bool
robustAround x = forall z .
  validImage z and lInfBall epsilon x z =>
  lInfBall delta (mnist x) (mnist z)

advises : Image -> Label -> Bool
advises x i = forall j . j != i => mnist x ! i < mnist x ! j

classificationRobustAround : Image -> Label -> Bool
classificationRobustAround x y = forall z .
  validImage z and lInfBall epsilon x z =>
  advises z y

--------------------------------------------------------------------------------
-- Robustness with respect to a dataset

-- We then ask for the dataset

parameter {n : Nat}

dataset trainingImages : Tensor Image [n]
dataset trainingLabels : Tensor Label [n]

robustness : Bool
robustness = forall separate x in trainingImages . robustAround x

classificationRobust : Bool
classificationRobust = forall separate i .
  robustAround (trainingData ! i) (trainingLabels ! i)