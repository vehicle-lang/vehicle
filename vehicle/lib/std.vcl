--------------------------------------------------------------------------------
-- Type annotations
--------------------------------------------------------------------------------

-- Implementation of the `:` syntax
typeAnn : forallT (t : Type) . t -> t
typeAnn t a = a

--------------------------------------------------------------------------------
-- Bool
--------------------------------------------------------------------------------

implies : Tensor Bool dims -> Tensor Bool dims -> Tensor Bool dims
implies x y = (not x) or y

forallInList : (A -> Bool) -> List A -> Bool
forallInList f xs = fold (\x y -> x and y) True (map f xs)

existsInList : (A -> Bool) -> List A -> Bool
existsInList f xs = fold (\x y -> x or y) False (map f xs)

--------------------------------------------------------------------------------
-- Tensor
--------------------------------------------------------------------------------

eqRatTensorReduced : Tensor Real dims -> Tensor Real dims -> Bool
eqRatTensorReduced xs ys = reduceAnd True (xs ==. ys)

neRatTensorReduced : Tensor Real dims -> Tensor Real dims -> Bool
neRatTensorReduced xs ys = not (eqRatTensorReduced xs ys)

leRatTensorReduced : Tensor Real dims -> Tensor Real dims -> Bool
leRatTensorReduced xs ys = reduceAnd True (xs <=. ys)

ltRatTensorReduced : Tensor Real dims -> Tensor Real dims -> Bool
ltRatTensorReduced xs ys = reduceAnd True (xs <. ys)

geRatTensorReduced : Tensor Real dims -> Tensor Real dims -> Bool
geRatTensorReduced xs ys = reduceAnd True (xs >=. ys)

gtRatTensorReduced : Tensor Real dims -> Tensor Real dims -> Bool
gtRatTensorReduced xs ys = reduceAnd True (xs >. ys)

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

existsIndex : forallT {n} . (Index n -> Bool) -> Bool
existsIndex f = reduceOr False (foreach i . f i)

forallIndex : forallT {n} . (Index n -> Bool) -> Bool
forallIndex f = reduceAnd True (foreach i . f i)
