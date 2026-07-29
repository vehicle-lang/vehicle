@network
classifier : Real -> Tensor Real [1]

bounded : Real -> Bool
bounded x = 0 <= x <= 1

@property
someProperty : Bool
someProperty = forall x . bounded x => (forall i . i != 1 => classifier x ! i <= 0)
