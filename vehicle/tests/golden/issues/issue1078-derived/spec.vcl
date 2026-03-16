-- Test that DerivedFunction builtins (e.g. forallIndex) are correctly
-- replaced when monomorphised with multiple different type arguments.
-- Uses `forall` over two different tensor sizes to trigger two distinct
-- monomorphisations of forallIndex (Index 3 and Index 5).

@network
f : Tensor Real [3] -> Tensor Real [5]

inputValid : Tensor Real [3] -> Bool
inputValid x = forall i . 0 <= x ! i <= 1

outputPositive : Tensor Real [5] -> Bool
outputPositive y = forall j . 0 <= y ! j

@property
p : Bool
p = forall x . inputValid x => outputPositive (f x)
