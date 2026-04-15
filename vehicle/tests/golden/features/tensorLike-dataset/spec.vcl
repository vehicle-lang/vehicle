@tensor
record Pair where
  { a : Real
  , b : Real
  }

@dataset
data : Pair

@network
f : Pair -> Pair
