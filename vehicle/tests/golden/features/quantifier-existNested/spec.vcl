@network
f: Real -> Real

@property
existEqualNested: Bool
existEqualNested = (exists x1 . 0 < x1 < 1 and f x1 >= 1) and (exists x2 . 1 < x2 < 2 and f x2 >= 2)

@property
existLeftMoreNested: Bool
existLeftMoreNested = ((exists x1 . 0 < x1 < 1 and f x1 >= 1) and (exists x2 . 1 < x2 < 2 and f x2 >= 2)) and (exists x3 . 2 < x3 < 3 and f x3 >= 3)

@property
existRightMoreNested: Bool
existRightMoreNested = (exists x1 . 0 < x1 < 1 and f x1 >= 1) and ((exists x2 . 1 < x2 < 2 and f x2 >= 2) and (exists x3 . 2 < x3 < 3 and f x3 >= 3))

