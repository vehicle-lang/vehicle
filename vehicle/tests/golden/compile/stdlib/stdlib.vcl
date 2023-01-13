{-
subVector : {{HasSub A}} -> Vector A n -> Vector A n -> Vector A n
subVector = zipWith (\x y -> x - y)

equalsVector : {{HasEq A}} -> Vector A n -> Vector A n -> Bool
equalsVector xs ys = bigAnd (zipWith (\x y -> x == y) xs ys)

notEqualsVector : {{HasEq A}} -> Vector A n -> Vector A n -> Bool
notEqualsVector xs ys = bigOr (zipWith (\x y -> x != y) xs ys)
-}
