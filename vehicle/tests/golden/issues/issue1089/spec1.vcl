@network
net : Tensor Real [1] -> Tensor Real [1]

@parameter
epsilon : Real

@property
boundedByEpsilon : Bool
boundedByEpsilon = forall x . -epsilon < x ! 0 < epsilon => -epsilon < net x ! 0 < epsilon
