@network
f : Tensor Real [1] -> Tensor Real [3]

@network
g : Tensor Real [1] -> Tensor Real [2]

-- Two finite quantifiers at different arities. The monomorphiser then emits the shared
-- `forallIndex` helper as its own declaration, named with the types it was specialised at.
twoOfThree : Tensor Real [1] -> Bool
twoOfThree x = forall (i : Index 3) . f x ! i >= 0.0

oneOfTwo : Tensor Real [1] -> Bool
oneOfTwo x = forall (i : Index 2) . g x ! i >= 0.5

@property
p : Bool
p = forall (x : Tensor Real [1]) . 0.0 <= x ! 0 <= 1.0 => (twoOfThree x and oneOfTwo x)
