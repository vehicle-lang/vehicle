@network
f : Tensor Real [1] -> Tensor Real [1]

@property
prop1 : Bool
prop1 = forall x . 0 < x < 1 => (if x > 0 then f [x] ! 0 > 0 else f [x] ! 0 <= 0)

@property
prop2 : Bool
prop2 = exists x . 0 < x < 1 and f [if x > 0 then x else 0.2] ! 0 >= 0

@property
prop3 : Bool
prop3 = exists x . 0 < x < 1 and (if f [x] ! 0 > 0 then x >= 0 else x < 0)
