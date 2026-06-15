@network
f: Real -> Real

@property
equalNested: Bool
equalNested = (forall x1 . f x1 >= 1) and (forall x2 . f x2 >= 2)
