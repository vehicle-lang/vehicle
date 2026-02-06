std : Vector Real 3
std = [0.2224, 0.2148, 0.2496]

-- normalisation: z = (x - mean) / std
normalise : Tensor Real [3, 1] -> Tensor Real [3, 1]
normalise x = foreach c w . (x ! c ! w) / (std ! c)

@network
classifier : Tensor Real [3, 1] -> Real

@property
oppositeFaces : Bool
oppositeFaces = forall perturbation .
  classifier (normalise perturbation) > 0
