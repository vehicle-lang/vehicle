-- Tests that records with different field types cannot be cast to tensors.

@tensor
record Test1 where
  { f1 : Real
  , f2 : Nat
  }
