@tensor
record TensorLikeRecord where
  { f1 : Real
  , f2 : Real
  }

record R where
  { a : Real
  , b : Tensor Real [4]
  , c : TensorLikeRecord
  }

@network
f : R -> R
