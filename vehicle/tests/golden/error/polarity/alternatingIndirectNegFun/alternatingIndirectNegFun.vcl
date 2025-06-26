@network
f : Tensor Real [1] -> Tensor Real [1]

myForall : Real -> Bool
myForall y = forall x . f x ! 0 >= y

notApp : (Real -> Bool) -> Real -> Bool
notApp (f : Real -> Bool) x = not (f x)

@property
p : Bool
p = forall y . notApp myForall y
