@network
f : Tensor Real [2] -> Tensor Real [3, 2]

trajectory : Tensor Real [3, 2]
trajectory = f [0.0, 0.0]

positions : Tensor Real [3]
positions = (transpose trajectory) ! 0

@property
positionsInRange : Bool
positionsInRange = forall t . 0.0 <= positions ! t <= 10.0

@property
positionUnderLimit : Bool
positionUnderLimit = positions <= const 10.0 [3]

@property
boundedScalar : Bool
boundedScalar = exists v . 0.0 <= v <= 1.0
                  and (forall t . positions ! t >= v)
