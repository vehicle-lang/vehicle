record R where
  { a : Real
  , b : Tensor Real [4]
  }

@network
f : R -> R
