@network
f : Tensor Real [2, 2] -> Tensor Real [1]

box : Tensor Real [2, 2, 2]
box = [[[0, 0.1], [1, 1.1]], [[0, 0.1], [0.5, 0.7]]]

@property
p : Bool
p = forall x . (forall i j . box ! i ! j ! 0 <= x ! i ! j <= 1) => f x ! 0 > 0
