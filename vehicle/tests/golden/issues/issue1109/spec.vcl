@network
classifier : Real -> Tensor Real [1]

@property
someProperty : Bool
someProperty = forall x . (forall i . i != 1 => classifier x ! i <= 0)
