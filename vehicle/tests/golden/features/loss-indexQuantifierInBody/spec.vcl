@network
f : Tensor Real [1] -> Tensor Real [1]

-- The inner quantifier does not mention `i`, so the body is compiled to a `const`, which
-- assertion compilation used to loop on.
@property
p : Bool
p =
  forall (y : Tensor Real [1]) .
    0.0 <= y ! 0 <= 1.0 => (forall (i : Index 1) . f y ! 0 >= y ! i)
