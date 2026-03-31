n = 6

type InputVector = Tensor Real [n]

checkRange : Index n -> Bool
checkRange i = 2 <= i <= 4

boundedSlice : InputVector -> Bool
boundedSlice x = forall i . checkRange i => 0.1 <= x ! i <= 0.2

validInput : InputVector -> Bool
validInput x = forall i . 0.0 <= x ! i <= 1.0

@property
indexOnlyQuantifier : Bool
indexOnlyQuantifier = forall x . validInput x and boundedSlice x => x ! 0 <= 1.0
