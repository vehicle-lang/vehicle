--------------------------------------------------------------------------------
-- Inputs and outputs

type Image = Tensor Real [2, 2]

type Label = Index 3

validImage : Image -> Bool
validImage x = forall i j . 0 <= x ! i ! j <= 1

--------------------------------------------------------------------------------
-- Network

@network
classifier : Image -> Tensor Real [3]

advises : Image -> Label -> Bool
advises x i = forall j . j != i => classifier x ! i > classifier x ! j

--------------------------------------------------------------------------------
-- Definition of robustness around a point

@parameter
epsilon : Real

boundedByEpsilon : Image -> Bool
boundedByEpsilon x = forall i j . -epsilon <= x ! i ! j <= epsilon

robustAround : Image -> Label -> Bool
robustAround image label = forall perturbation .
  let perturbedImage = image - perturbation in
  boundedByEpsilon perturbation and validImage perturbedImage =>
    advises perturbedImage label

--------------------------------------------------------------------------------
-- Robustness with respect to a dataset

n : Nat
n = 1

trainingImages : Vector Image n
trainingImages = [[[1,2],[3,4]]]

trainingLabels : Vector Label n
trainingLabels = [0]

@property
robust : Vector Bool n
robust = foreach i . robustAround (trainingImages ! i) (trainingLabels ! i)
