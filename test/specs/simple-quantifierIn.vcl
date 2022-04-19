emptyList : List Int
emptyList = []

empty : Prop
empty = forall x in emptyList . True

double : Prop
double = forall x in emptyList . forall y in emptyList . x == y

forallForallIn : Prop
forallForallIn = forall x . forall y in emptyList . x == y

forallInForall : Prop
forallInForall = forall x in emptyList . forall y . x == y