type Input = Tensor Real [4]

sepalLength = 0
sepalWidth = 1
petalLength = 2
petalWidth = 3

type Output = Tensor Real [3]

setosa = 0
versicolor = 1
virginica = 2


-- There is no embedding gap?

@network
iris : Input -> Output


minimumInputValues : Input
minimumInputValues = [4, 1.5, 0.5, 0.01]

maximumInputValues : Input
maximumInputValues = [8, 5, 7, 3]

validInput : Input -> Bool
validInput x = forall i .
  minimumInputValues ! i <= x ! i <= maximumInputValues ! i


hasAns : Output -> Bool
hasAns x = exists d . (forall d2 . d != d2 => x ! d > x ! d2)


isSetosa : Output -> Bool
isSetosa x = forall d . d != setosa => x ! setosa > x ! d

isVersicolor : Output -> Bool
isVersicolor x = forall d . d != versicolor => x ! versicolor > x ! d

isVirginica : Output -> Bool
isVirginica x = forall d . d != virginica => x ! virginica > x ! d


-- Test property
@property
testProperty : Bool
testProperty = forall x . validInput x => hasAns (iris x)
