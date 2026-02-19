@tensor
record Pair where
  { a : Real
  , b : Real
  }

@network
f : Pair -> Tensor Real [5]
