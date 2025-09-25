-- Tests that records with invalid field types cannot be cast to tensors.

record R1 where
  { f1 : Real
  , f2 : Real
  }

@tensor
record Test2 where
  { f3 : R1
  , f4 : R1
  }
