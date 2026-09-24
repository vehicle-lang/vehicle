@network
f : Tensor Real [1] -> Tensor Real [1]

-- Each declaration mixes a decidable `Bool` with a loss value, which is the case the loss
-- backend compiles to a `where` rather than to one of the logic's own operations.
--
-- Unrolling `forall i : Index 2` gives i=0, where the decidable side is False, and i=1, where
-- it is True. Each declaration therefore reduces to a statement about `f` alone, which is what
-- makes it testable by varying only the network.

-- i=0 gives `not (False and L)` = True, i=1 gives `not (True and L)` = not L.
mixedAnd : Tensor Real [1] -> Bool
mixedAnd x = forall (i : Index 2) . not ((i != 0) and (f x ! 0 >= 0.5))

-- i=0 gives `False or L` = L, i=1 gives `True or L` = True.
mixedOr : Tensor Real [1] -> Bool
mixedOr x = forall (i : Index 2) . (i != 0) or (f x ! 0 >= 0.5)

-- i=0 gives `L => False` = not L, i=1 gives `L => True` = True.
mixedImpliesBool : Tensor Real [1] -> Bool
mixedImpliesBool x = forall (i : Index 2) . f x ! 0 >= 0.5 => i != 0

@property
p : Bool
p = forall (x : Tensor Real [1]) . 0.0 <= x ! 0 <= 1.0 =>
  (mixedAnd x and mixedOr x and mixedImpliesBool x)
