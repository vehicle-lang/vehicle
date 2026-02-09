@network
bug : Tensor Real [2] -> Real

@property
safeFar : Bool
safeFar = exists x . 0 <= x ! 0 <= 1 and 36.5 <= x ! 1 <= 40 and x ! 0 >= 31 and bug x < 1
