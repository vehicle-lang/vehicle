-- Tests that parameterised records cannot be cast to tensors.
@tensor
record Pair a where
  { x : a
  , y : a
  }
