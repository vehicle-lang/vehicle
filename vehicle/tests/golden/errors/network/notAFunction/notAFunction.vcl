type Image = Tensor Real [3, 28, 28]

face1 = 0
face2 = 1
face3 = 2
face4 = 3
face5 = 4
face6 = 5

@network
classifier : Image -> Tensor Real [6]

predicts : Image -> Index 6 -> Bool
predicts x i =
  let y = classifier x in
    y ! i > 0

@parameter(infer=True)
n : Nat

@dataset
images : Vector Image n

notBoth : Image -> Index 6 -> Index 6 -> Bool
notBoth image a b = not ( (predicts image a) and (predicts image b) )

oneOrOther : Image -> Index 6 -> Index 6 -> Bool
oneOrOther image a b = (predicts image a) or (predicts image b)

oppositeFaces : Image -> Bool
oppositeFaces image = forall (perturbation : Image) .
  let perturbedImage = image - perturbation in
      ( (notBoth perturbedImage face1 face6) and (oneOrOther perturbedImage face1 face6) ) and
      ( (notBoth perturbedImage face2 face5) and (oneOrOther perturbedImage face2 face5) ) and
      ( (notBoth perturbedImage face3 face4) and (oneOrOther perturbedImage face3 face4) )

@property
robust : Vector Bool n
robust = foreach i . oppositeFaces (images ! i)
