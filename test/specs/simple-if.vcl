network f : Rat -> Rat

prop1 : Prop
prop1 = forall x . if x > 0 then f x > 0 else f x <= 0

-- prop2 : Prop
-- prop2 = exists x . f (if x > 0 then x else 0.2) >= 0
--
-- prop3 : Prop
-- prop3 = exists x . if f x > 0 then x >= 0 else x < 0



-- Re-enable once tensors have been sorted out.

-- network g : Rat -> Tensor 2 [Rat]
--
-- prop4 : Prop
-- prop4 = exists x . (map (\v -> if v > 0 then 1 else 2) (g x)) ! 0 > 0