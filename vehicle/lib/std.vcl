--------------------------------------------------------------------------------
-- Type annotations
--------------------------------------------------------------------------------

-- Implementation of the `:` syntax
typeAnn : forallT (t : Type) . t -> t
typeAnn t a = a

--------------------------------------------------------------------------------
-- Bool
--------------------------------------------------------------------------------

notBoolOp2 : (A -> B -> Bool) -> (A -> B -> Bool)
notBoolOp2 f x y = not (f x y)

--------------------------------------------------------------------------------
-- Orderings
--------------------------------------------------------------------------------

min : {{HasLeq a a}} -> a -> a -> a
min x y = if x <= y then x else y

max : {{HasLeq a a}} -> a -> a -> a
max x y = if y <= x then x else y

--------------------------------------------------------------------------------
-- List
--------------------------------------------------------------------------------

vectorToList : forallT {@0 n} {A} . Vector A n -> List A
vectorToList = fold (\x xs -> x :: xs) nil

--------------------------------------------------------------------------------
-- Foldable
--------------------------------------------------------------------------------

bigAnd : forallT {f : Type -> Type} . {{HasFold f}} -> f Bool -> Bool
bigAnd = fold (\x y -> x and y) True

bigOr : forallT {f : Type -> Type} . {{HasFold f}} -> f Bool -> Bool
bigOr = fold (\x y -> x or y) False

forallIn : forallT {t : Type -> Type} . {{HasFold t}} -> {{HasMap t}} -> (A -> Bool) -> t A -> Bool
forallIn f xs = bigAnd (map f xs)

existsIn : forallT {t : Type -> Type} . {{HasFold t}} -> {{HasMap t}} -> (A -> Bool) -> t A -> Bool
existsIn f xs = bigOr (map f xs)

--------------------------------------------------------------------------------
-- Vector
--------------------------------------------------------------------------------

vectorToVector : forallT {@0 n} {A} . Vector A n -> Vector A n
vectorToVector xs = xs

foreachIndex : forallT n . (Index n -> A) -> Vector A n
foreachIndex n f = map f (indices n)

addVector : forallT {@0 n} . {{HasAdd A B C}} -> Vector A n -> Vector B n -> Vector C n
addVector = zipWith (\x y -> x + y)

subVector : forallT {@0 n} . {{HasSub A B C}} -> Vector A n -> Vector B n -> Vector C n
subVector = zipWith (\x y -> x - y)

equalsVector : forallT {@0 n} . {{HasEq A B}} -> Vector A n -> Vector B n -> Bool
equalsVector xs ys = bigAnd (zipWith (\x y -> x == y) xs ys)

notEqualsVector : forallT {@0 n} . {{HasNotEq A B}} -> Vector A n -> Vector B n -> Bool
notEqualsVector xs ys = bigOr (zipWith (\x y -> x != y) xs ys)

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

existsIndex : forallT n . (Index n -> Bool) -> Bool
existsIndex n f = bigOr (foreachIndex n f)

forallIndex : forallT n . (Index n -> Bool) -> Bool
forallIndex n f = bigAnd (foreachIndex n f)

--------------------------------------------------------------------------------
-- Tensor
--------------------------------------------------------------------------------

Tensor : Type -> List Nat -> Type
Tensor A ds = fold (\d t -> Vector t d) A ds
