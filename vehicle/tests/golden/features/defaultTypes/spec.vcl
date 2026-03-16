-- Tests that `1` is properly assigned a default type of `Index 2`
test : Bool
test = forall (i : Index 5) . i <= 1
