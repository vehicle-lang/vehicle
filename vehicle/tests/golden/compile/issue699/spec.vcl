type Image = Tensor Rat [1, 1]

type Label = Index 10

validImage : Image -> Bool
validImage x = forall i j . 0 <= x ! i ! j <= 1

@network
classifier : Image -> Vector Rat 10

scaler : Image -> Rat
scaler x = classifier x ! 0 + classifier x ! 1 + classifier x ! 2 + classifier x ! 3 + classifier x ! 4 + classifier x ! 5 + classifier x ! 6 + classifier x ! 7 + classifier x ! 8 + classifier x ! 9

scaleCOutput : Image -> Label -> Rat
scaleCOutput x i = ((classifier x) ! i)  / (scaler x)

advises : Image -> Label -> Bool
advises x i = scaleCOutput x i > 0.5

epsilon : Rat
epsilon = 0.5

boundedByEpsilon : Image -> Bool
boundedByEpsilon x = forall i j . -epsilon <= x ! i ! j <= epsilon

robustAround : Image -> Label -> Bool
robustAround image label = forall pertubation .
  let perturbedImage = image - pertubation in
  boundedByEpsilon pertubation and validImage perturbedImage =>
    advises perturbedImage label

@property
robust : Bool
robust = robustAround [[0.5]] 0
