type Image = Tensor Real [1]

@network
classifier : Image -> Tensor Real [1]

@property
robustAround : Bool
robustAround = forall x . 1 / (classifier x ! 0) > 0.5
