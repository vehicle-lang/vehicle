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


exampleR : R
exampleR =
  { a = 4
  , b = [1, 2, 3, 4]
  , c = { f1 = 5, f2 = 6 }
  }

@property
exampleP : Bool
exampleP = (f exampleR).a > exampleR.a
{-
  (f exampleR) generates:
      exampleR   = x
      f exampleR = y

  this also requires that:
      exampleR.a = x.a
      exampleR.b = x.b
      exampleR.c = x.c

  and so on, for all the levels in the tensors:
      exampleR.b[0] = x.b[0]
      ...
-}
