-- Chained pointwise tensor comparisons. Mirrors the scalar chained
-- form `lo < x < hi` but for the pointwise operators `<.`, `<=.`,
-- `>.`, `>=.`, `==.`. Mixed strictness like `<. <=.` is allowed
-- (same direction).

@network
net : Tensor Real [3] -> Tensor Real [3]

signal : Tensor Real [3]
signal = net [0.0, 0.0, 0.0]

@property
strictlyBounded : Bool
strictlyBounded = (const 0.0 [3] <. signal <. const 1.0 [3]) ! 0

@property
mixedStrictness : Bool
mixedStrictness = (const 0.0 [3] <. signal <=. const 1.0 [3]) ! 0

@property
upperFirst : Bool
upperFirst = (const 1.0 [3] >. signal >=. const 0.0 [3]) ! 0

@property
threeWay : Bool
threeWay =
  ((const 0.0 [3] <. signal <. const 0.5 [3] <. const 1.0 [3]) ! 0)
