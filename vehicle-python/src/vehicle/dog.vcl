type Image = Tensor Rat [28, 28]

type Label = Index 10

validImage : Image -> Bool
validImage x = forall i j . 0 <= x ! i ! j <= 1

@network
f : Image -> Vector Rat 10

advises : Image -> Label -> Bool
advises x i = forall j . j != i => f x ! i > f x ! j

advisesSecond : Image -> Label -> Bool
advisesSecond x i = forall j . j != i => f x ! i > f x ! j

@parameter
firstClass : Nat
@parameter
secondClass : Nat

@property
dog : Bool
dog = forall x . validImage x => advises x firstClass and not (advisesSecond x secondClass)