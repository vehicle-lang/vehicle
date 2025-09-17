-- Tests that records with invalid field types cannot be cast to tensors.

record Incorrect where
  { f1 : Real
  , f2 : Real
  }

@tensor
record Test2 where
  { f3 : Incorrect
  , f4 : Incorrect
  }
