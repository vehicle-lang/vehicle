polyId : forall t. t -> t
polyId = \{t} -> \x -> x

realId : Real -> Real
realId = polyId {Real}

intId : Int -> Int
intId = polyId {Int}

real : Real
real = polyId {Real} 0.0

int : Int
int = polyId {Int} 0

tensorId : forall t. Tensor t [2] -> Tensor t [2]
tensorId {t} = polyId {Tensor t [2]}
