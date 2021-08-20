polyId : forall (t : Type 0). t -> t
polyId = \t x -> x

polyIdImplicit : forall {t : Type 0}. t -> t
polyIdImplicit x = x

realId : Real -> Real
realId = polyId Real

-- TODO: insert implicit lambdas even when there is no lambda before it
polyIdImplicitTwo : forall {t : Type 0}. t -> t
polyIdImplicitTwo {t} = polyIdImplicit

realIdImplicit : Real -> Real
realIdImplicit x = polyIdImplicit x

intId : Int -> Int
intId x = polyIdImplicit x



{-
real : Real
real = polyId {Real} 0.0

int : Int
int = polyId {Int} 0

tensorId : forall t. Tensor t [2] -> Tensor t [2]
tensorId {t} = polyId {Tensor t [2]}
-}
