type Image = Tensor Real [2, 1]

normalise : Image -> Image
normalise x = foreach c . x ! c

@network
classifier : Image -> Tensor Real [1]

@property
robust : Bool
robust = forall x . classifier (normalise x) ! 0 > 0
