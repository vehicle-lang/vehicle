@network
f: Real -> Real

@property
rightMoreNested: Bool
rightMoreNested = (forall x1 . f x1 >= 1) and ((forall x2 . f x2 >= 2) and (forall x3 . f x3 >= 3))
