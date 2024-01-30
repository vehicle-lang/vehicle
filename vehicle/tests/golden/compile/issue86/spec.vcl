@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p1 : Bool
p1 = forall x . forall y in f x . y > 0
{-
@property
p2 : Bool
p2 = fold (\x r -> x > 0 and r) True (f [0])
-}
@property
p3 : Bool
p3 = forall x . fold (\a b -> a > 0 and b) True (f [if f [x] ! 0 > 0 then 0 else x])
