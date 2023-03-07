emptyList : List Int
emptyList = []

@property
empty : Bool
empty = forall x in emptyList . True

@property
double : Bool
double = forall x in emptyList . forall y in emptyList . x == y

@property
forallForallIn : Bool
forallForallIn = forall x . forall y in emptyList . x == y

@property
forallInForall : Bool
forallInForall = forall x in emptyList . forall y . x == y
