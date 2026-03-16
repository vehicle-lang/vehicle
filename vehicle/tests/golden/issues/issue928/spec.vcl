type Image = Tensor Real [2, 1]

normalise : Image -> Image
normalise x = foreach c . x ! c

@network
classifier : Image -> Tensor Real [1]

@property
robust : Bool
robust = forall x . [[0], [0]] < x <= [[1], [1]] => classifier (normalise x) ! 0 > 0
