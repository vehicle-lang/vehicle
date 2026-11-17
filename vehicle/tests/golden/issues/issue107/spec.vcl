-- Quantifiers may be used as the right-hand operand of boolean connectives
-- without brackets, and their bodies extend as far to the right as possible.

@property
conjunction : Bool
conjunction = 1 >= 0 and exists (x : Index 2) . x >= 0

@property
disjunction : Bool
disjunction = 1 >= 0 or forall (x : Index 2) . x >= 0

@property
implication : Bool
implication = 1 >= 0 => exists (x : Index 2) . x >= 0

@property
chained : Bool
chained = 1 >= 0 and exists (x : Index 2) . x >= 0 and forall (y : Index 2) . y >= x => x <= y

@property
quantifiedIn : Bool
quantifiedIn = 1 >= 0 and forall x in [1, 2, 3] . x >= (0 : Nat)
