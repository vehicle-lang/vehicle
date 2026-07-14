@network
f: Real -> Real

@property
forallEqualNested: Bool
forallEqualNested = (forall x1 . 0 < x1 < 1 => f x1 >= 1) and (forall x2 . 1 < x2 < 2 => f x2 >= 2)

{-
@property
forallLeftMoreNested: Bool
forallLeftMoreNested = ((forall x1 . 0 < x1 < 1 => f x1 >= 1) and (forall x2 . 1 < x2 < 2 => f x2 >= 2)) and (forall x3 . 2 < x3 < 3 => f x3 >= 3)

@property
forallRightMoreNested: Bool
forallRightMoreNested = (forall x1 . 0 < x1 < 1 => f x1 >= 1) and ((forall x2 . 1 < x2 < 2 => f x2 >= 2) and (forall x3 . 2 < x3 < 3 => f x3 >= 3))
-}