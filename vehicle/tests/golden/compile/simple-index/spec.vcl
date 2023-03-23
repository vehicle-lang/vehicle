-- Indices

@property
eqIndex : Bool
eqIndex = (0 : Index 1) == (1 : Index 2)

@property
neqIndex : Bool
neqIndex = (0 : Index 1) != (1 : Index 2)

@property
leqIndex : Bool
leqIndex = (0 : Index 1) <= (1 : Index 2)

@property
ltIndex : Bool
ltIndex = (0 : Index 1) < (1 : Index 2)

@property
geqIndex : Bool
geqIndex = (0 : Index 1) >= (1 : Index 2)

@property
gtIndex : Bool
gtIndex = (0 : Index 1) > (1 : Index 2)