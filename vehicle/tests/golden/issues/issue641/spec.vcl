type Image = Tensor Real [28, 28]

@network
classifier : Image -> Tensor Real [10]

@dataset
images : Vector Image 1

@property
p : Bool
p = let scores = classifier (images ! 0) in
    scores ! 7 > 0 or not(scores ! 7 > 0)
