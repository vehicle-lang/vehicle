--------------------------------------------------------------------------------
-- Doggos

numberOfDogs = 6

type Dog = Index numberOfDogs

unknownDog     = 0

greatDane      = 1
germanShepherd = 2

chihuahua  = 4
pekinese   = 5

smallDogs : List Dog
smallDogs = [chihuahua, pekinese]

bigDogs : List Dog
bigDogs = [greatDane, germanShepherd]

--------------------------------------------------------------------------------
-- Network

type Image = Tensor Rat [4, 4]
type Score = Rat

@network
score : Image -> Vector Score numberOfDogs

validPixel : Rat -> Bool
validPixel p = 0 <= p <= 1

validImage : Image -> Bool
validImage x = forall i j . validPixel (x ! i ! j)

--------------------------------------------------------------------------------
-- Predicates

isFirstChoice : Image -> Dog -> Bool
isFirstChoice x dog1 =
  let scores = score x in
  forall d . d != dog1 => scores ! dog1 > scores ! d

isSecondChoice : Image -> Dog -> Bool
isSecondChoice x dog2 =
  let scores = score x in
  exists dog1 . (isFirstChoice x dog1) and (forall d . d != dog1 and d != dog2 => scores ! dog2 > scores ! d)

noConfusionWith : Image -> List Dog -> List Dog -> Bool
noConfusionWith x dogs1 dogs2 =
  forall dog1 in dogs1 .
    forall dog2 in dogs2 .
      not (isFirstChoice x dog1 and isSecondChoice x dog2)


-------------------------------------------------------------------------------
-- Properties

@property
doesNotConfuseBigAndSmall : Bool
doesNotConfuseBigAndSmall =
  forall x . validImage x => noConfusionWith x bigDogs smallDogs
