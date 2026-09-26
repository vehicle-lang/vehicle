@network
f : Tensor Real [1] -> Tensor Real [1]

-- `f` is the identity in the tests, so the body's loss over [0, 1] is worst at
-- `x = 0` and best at `x = 1`.
@property
universal : Bool
universal = forall (x : Real) . 0.0 <= x <= 1.0 => f [x] ! 0 >= 0.5

@property
existential : Bool
existential = exists (x : Real) . 0.0 <= x <= 1.0 and f [x] ! 0 >= 0.5
