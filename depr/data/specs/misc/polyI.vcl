polyId : forall (t : Type 0). t -> t
polyId = \t x -> x

polyIdImplicit : forall {t : Type 0}. t -> t
polyIdImplicit x = x

realId : Rat -> Rat
realId = polyId Rat

polyIdImplicitTwo : forall {t : Type 0}. t -> t
polyIdImplicitTwo = polyIdImplicit

realIdImplicit : Rat -> Rat
realIdImplicit x = polyIdImplicit x

intId : Int -> Int
intId x = polyIdImplicit x



{-
real : Rat
real = polyId {Rat} 0.0

int : Int
int = polyId {Int} 0

tensorId : forall t. Tensor t [2] -> Tensor t [2]
tensorId {t} = polyId {Tensor t [2]}
-}
