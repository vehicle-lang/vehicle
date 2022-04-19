
emptyList : List Int
emptyList = []

empty : Prop
empty = forall x in emptyList . True

double : Prop
double = forall x in emptyList . forall y in emptyList . x == y