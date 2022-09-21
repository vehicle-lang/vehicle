@network
f : Tensor Rat [1] -> Tensor Rat [1]

myForall : Rat -> Bool
myForall y = forall x . f x ! 0 >= y

notApp : (Rat -> Bool) -> Rat -> Bool
notApp (f : Rat -> Bool) x = not (f x)

@property
p : Bool
p = forall y . notApp myForall y


-- f : (Rat -> Rat) -> Rat -> Rat
-- f (h : Rat -> Rat) x = h x
--
-- g : Rat -> Rat
-- g x = x + 1
--
-- x : Rat
-- x = f g 0