-- original RGB image (3 channels, CHW) with pixel values in [0, 1]
type UnnormalisedImage = Tensor Real [3, 28, 28]

-- image normalised with mean / std normalisation (common when training computer vision networks)
type NormalisedImage = Tensor Real [3, 28, 28]

-- normalisation:
mean : Vector Real 3
mean = [0.7469, 0.7385, 0.6633]

std : Vector Real 3
std = [0.2224, 0.2148, 0.2496]

-- normalisation: z = (x - mean) / std
normalise : UnnormalisedImage -> NormalisedImage
normalise x =
  foreach c .
    foreach h .
      foreach w .
        let m = mean ! c in
        let s = std ! c in
          ( (x ! c ! h ! w) - m ) / s

-- denormalisation: x = z * std + mean
denormalise : NormalisedImage -> UnnormalisedImage
denormalise x =
  foreach c .
    foreach h .
      foreach w .
        let m = mean ! c in
        let s = std ! c in
          ( (x ! c ! h ! w) * s ) + m

-- pixel values in [0,1]
validImage : UnnormalisedImage -> Bool
validImage x = forall c h w . 0 <= x ! c ! h ! w <= 1

-- network outputs 6 labels corresponding to the 6 faces of a dice
face1 = 0
face2 = 1
face3 = 2
face4 = 3
face5 = 4
face6 = 5

@network
classifier : NormalisedImage -> Tensor Real [6]

-- a face is predicted if the logit is > 0
predicts : UnnormalisedImage -> Index 6 -> Bool
predicts x i =
  let z = normalise x in
  let y = classifier z in
    y ! i > 0

@parameter
epsilon : Real

boundedByEpsilon : UnnormalisedImage -> Bool
boundedByEpsilon x = forall c h w . -epsilon <= x ! c ! h ! w <= epsilon

@parameter(infer=True)
n : Nat

@dataset
images : Vector UnnormalisedImage n

-- not both opposite faces (e.g. 1 and 6) can be visible at the same time
notBoth : UnnormalisedImage -> Index 6 -> Index 6 -> Bool
notBoth image a b = not ( (predicts image a) and (predicts image b) )

-- since all images show 3 faces of the dice, exactly one of each opposing face pairs needs to be visible
oneOrOther : UnnormalisedImage -> Index 6 -> Index 6 -> Bool
oneOrOther image a b = (predicts image a) or (predicts image b)

oppositeFaces : UnnormalisedImage -> Bool
oppositeFaces image = forall (perturbation : UnnormalisedImage) .
  let perturbedImage = image - perturbation in
    boundedByEpsilon perturbation and validImage perturbedImage =>
      ( (notBoth perturbedImage face1 face6) and (oneOrOther perturbedImage face1 face6) ) and
      ( (notBoth perturbedImage face2 face5) and (oneOrOther perturbedImage face2 face5) ) and
      ( (notBoth perturbedImage face3 face4) and (oneOrOther perturbedImage face3 face4) )

@property
robust : Vector Bool n
robust = foreach i . oppositeFaces (images ! i)
