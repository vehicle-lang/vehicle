@network
f : Tensor Real [4] -> Tensor Real [4]

predicted : Tensor Real [4]
predicted = f [0.0, 0.0, 0.0, 0.0]

@property
belowOne : Bool
belowOne = forall x . 0 < x < 1 => (f [x, x, x, x]) ! 0 <= 1.0
