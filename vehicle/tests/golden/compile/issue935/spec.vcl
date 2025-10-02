type UnnormalisedImage = Tensor Real [2, 3]

@network
classifier : UnnormalisedImage -> Real

boundedByEpsilon : UnnormalisedImage -> Bool
boundedByEpsilon x = forall c h . 0 <= x ! c ! h <= 1

@property
robust : Bool
robust = forall x . boundedByEpsilon x => classifier x > 0
