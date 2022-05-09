emptyList : List Int
emptyList = []

empty : Bool
empty = forall x in emptyList . True

double : Bool
double = forall x in emptyList . forall y in emptyList . x == y

forallForallIn : Bool
forallForallIn = forall x . forall y in emptyList . x == y

forallInForall : Bool
forallInForall = forall x in emptyList . forall y . x == y