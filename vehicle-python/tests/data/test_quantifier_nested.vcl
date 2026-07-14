@network
f: Real -> Real

@property
equalNested: Bool
equalNested = (exists x1 . 0 < x1 < 1 and f x1 >= 1) and (exists x2 . 1 < x2 < 2 and f x2 >= 2)

@property
leftMoreNested: Bool
leftMoreNested = ((exists x1 . 0 < x1 < 1 and f x1 >= 1) and (exists x2 . 1 < x2 < 2 and f x2 >= 2)) and (exists x3 . 2 < x3 < 3 and f x3 >= 3)

@property
rightMoreNested: Bool
rightMoreNested = (exists x1 . 0 < x1 < 1 and f x1 >= 1) and ((exists x2 . 1 < x2 < 2 and f x2 >= 2) and (exists x3 . 2 < x3 < 3 and f x3 >= 3))