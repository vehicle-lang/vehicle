type NormalisedImage = Tensor Real [1, 1]

normalise : NormalisedImage -> NormalisedImage
normalise x = foreach c . foreach h . x ! c ! h

@network
classifier : NormalisedImage -> Tensor Real [6]

@property
oppositeFaces : Bool
oppositeFaces = forall perturbation . classifier (normalise perturbation) ! 0 > 0
