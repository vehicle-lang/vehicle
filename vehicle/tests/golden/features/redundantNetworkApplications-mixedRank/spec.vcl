-- Tests that redundant network applications are detected when the
-- equalities relating their inputs are stated at different ranks:
--   f₀[output] == f₁[input]         (whole tensor)
--   f₀[output] ! i == f₂[input] ! i  (individual elements)
-- Once `f y` is identified with `f (f x)` the property is trivially true.

@network
f : Tensor Real [2] -> Tensor Real [2]

@property
p : Bool
p = forall x y . y ! 0 == f x ! 0 and y ! 1 == f x ! 1 => f (f x) ! 0 >= f y ! 0
