{-

addVector : {{HasAdd A}} -> Vector A n -> Vector A n -> Vector A n
addVector = zipWith (\x y -> x + y)

bigAnd : Vector Bool n -> Bool
bigAnd = fold (\x y -> x and y) True

bigOr : Vector Bool n -> Bool
bigOr = fold (\x y -> x or y) False

bigAnd2 : Vector Bool n -> Bool
bigAnd2 = fold (\x y -> x and y) True

subVector : {{HasSub A}} -> Vector A n -> Vector A n -> Vector A n
subVector = zipWith (\x y -> x - y)

equalsVector : {{HasEq A}} -> Vector A n -> Vector A n -> Bool
equalsVector xs ys = bigAnd (zipWith (\x y -> x == y) xs ys)

notEqualsVector : {{HasEq A}} -> Vector A n -> Vector A n -> Bool
notEqualsVector xs ys = bigOr (zipWith (\x y -> x != y) xs ys)

forallIndex : (Index n -> Bool) -> Bool
forallIndex f = bigAnd (foreach i . f i)

existsIndex : (Index n -> Bool) -> Bool
existsIndex f = bigOr (foreach i . f i)

tensor : Type -> [Nat] -> Type
tensor A dims = fold (\a b -> Vector b a) A dims
-}
