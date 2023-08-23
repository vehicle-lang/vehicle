type Image = Tensor Rat [1]

@network
classifier : Image -> Vector Rat 1

@property
robustAround : Bool
robustAround = forall x . 1 / (classifier x ! 0) > 0.5
