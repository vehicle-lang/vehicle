type Image = Tensor Rat [28, 28]

@network
classifier : Image -> Vector Rat 10

@dataset
images : Vector Image 1

@property
p : Bool
p = let scores = classifier (images ! 0) in
    scores ! 7 > 0 or not(scores ! 7 > 0)
