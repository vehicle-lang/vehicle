-- original RGB image (3 channels, CHW) with pixel values in [0, 1]
type ImageChannel = Tensor Real [28, 28]

-- normalisation:
channelBoundedBy : Real -> Real -> ImageChannel -> Bool
channelBoundedBy lower upper x = forall i j . lower <= x ! i ! j <= upper

normaliseChannel : Real -> Real -> ImageChannel -> ImageChannel
normaliseChannel mean std x =
  foreach i .
    foreach j .
      let m = mean in
      let s = std in
        ( (x ! i ! j) - m ) / s

@tensor
record Image where
  { r : ImageChannel
  , g : ImageChannel
  , b : ImageChannel
  }

imageBoundedBy : Real -> Real -> Image -> Bool
imageBoundedBy lower upper image = 
  channelBoundedBy lower upper (image.r) and 
  channelBoundedBy lower upper (image.g) and 
  channelBoundedBy lower upper (image.b)

means : Vector Real 3
means = [0.7469, 0.7385, 0.6633]

stdDev : Vector Real 3
stdDev = [0.2224, 0.2148, 0.2496]

normaliseImage : Image -> Image
normaliseImage image =
  { r = normaliseChannel (means ! 0) (stdDev ! 0) (image.r)
  , g = normaliseChannel (means ! 1) (stdDev ! 1) (image.g)
  , b = normaliseChannel (means ! 2) (stdDev ! 2) (image.b)
  }

-- network outputs 6 labels corresponding to the 6 faces of a dice
face1 = 0
face2 = 1
face3 = 2
face4 = 3
face5 = 4
face6 = 5

@network
classifier : Image -> Tensor Real [6]

-- a face is predicted if the logit is > 0
predicts : Image -> Index 6 -> Bool
predicts x i =
  let z = normaliseImage x in
  let y = classifier z in
    y ! i > 0

@parameter
epsilon : Real

-- not both opposite faces (e.g. 1 and 6) can be visible at the same time
notBoth : Image -> Index 6 -> Index 6 -> Bool
notBoth image x y = not ( (predicts image x) and (predicts image y) )

-- since all images show 3 faces of the dice, exactly one of each opposing face pairs needs to be visible
oneOrOther : Image -> Index 6 -> Index 6 -> Bool
oneOrOther image x y = (predicts image x) or (predicts image y)

oppositeFaces : Image -> Bool
oppositeFaces image = forall (perturbation : Image) .
  let perturbedImage = image - perturbation in
    imageBoundedBy (-epsilon) epsilon perturbation and imageBoundedBy 0 1 perturbedImage =>
      ( (notBoth perturbedImage face1 face6) and (oneOrOther perturbedImage face1 face6) ) and
      ( (notBoth perturbedImage face2 face5) and (oneOrOther perturbedImage face2 face5) ) and
      ( (notBoth perturbedImage face3 face4) and (oneOrOther perturbedImage face3 face4) )

@parameter(infer=True)
n : Nat

@dataset
images : Vector Image n

@property
robust : Vector Bool n
robust = foreach i . oppositeFaces (images ! i)
