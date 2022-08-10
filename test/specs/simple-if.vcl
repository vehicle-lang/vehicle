@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
prop1 : Bool
prop1 = forall x . if x > 0 then f [x] ! 0 > 0 else f [x] ! 0 <= 0

-- @property
-- prop2 : Bool
-- prop2 = exists x . f [if x > 0 then x else 0.2] >= 0

@property
prop3 : Bool
prop3 = exists x . if f [x] ! 0 > 0 then x >= 0 else x < 0

-- Re-enable once tensors have been sorted out.

-- @network
-- g : Rat -> Tensor Rat [2]
--
-- @property
-- prop4 : Bool
-- prop4 = exists x . (map (\v -> if v > 0 then 1 else 2) (g x)) ! 0 > 0