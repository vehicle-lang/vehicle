-- @tensor
-- record Pair where
--   { a : Real
--   , b : Real
--   }

type Pair = Tensor Real [3]

@parameter(infer=True)
n : Nat

@dataset
data : Vector Pair n

@network
f : Pair -> Pair

@property
p : Vector Bool n
p = foreach i . (f (data ! i)) ! 0 > 1
