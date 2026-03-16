@network
pk : Real -> Tensor Real [1]

@property
example : Bool
example = forall x . (pk x) ! 0 <= 5
