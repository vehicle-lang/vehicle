@network
f: Real -> Real

@property
equalNested: Bool
equalNested = (exists x1 . 0 < x1 < 1 and f x1 >= 1) and (exists x2 . 1 < x2 < 2 and f x2 >= 2)
