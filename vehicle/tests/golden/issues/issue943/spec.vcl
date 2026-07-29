type Input = Tensor Real [2]

@network
classifier : Input -> Real

boundedByEpsilon : Input -> Bool
boundedByEpsilon x = reduceAnd ([ -3.25, -3.25 ] <=. x and x <=. [ 3.25, 3.25 ])

@property
robust : Bool
robust = forall x . boundedByEpsilon x => classifier x > 0
