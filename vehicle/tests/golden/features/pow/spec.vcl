-- Regression test: numeric literals should be accepted as the exponent of `^`.

@tensor
record Position where
  { x : Real
  , y : Real
  , z : Real
  } supports Addition

euclideanNorm : Position -> Real
euclideanNorm p = (p.x^2 + p.y^2 + p.z^2)^0.5

@network
f : Tensor Real [2] -> Tensor Real [2]

@property
bounded : Bool
bounded = forall v . 0 <= v ! 0 <= 1 => (f v ^ 2) ! 0 <= 1
