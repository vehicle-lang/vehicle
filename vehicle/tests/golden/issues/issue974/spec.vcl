-- An `if` may be used as the right-hand operand of boolean connectives
-- without brackets, and its `else` branch extends as far to the right as
-- possible.

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
implication : Bool
implication = forall x . 0 < x < 1 => if x > 0 then f [x] ! 0 > 0 else f [x] ! 0 <= 0

@property
conjunction : Bool
conjunction = forall x . 0 < x < 1 and if x > 0 then f [x] ! 0 > 0 else f [x] ! 0 <= 0

@property
disjunction : Bool
disjunction = forall x . 0 < x < 1 or if x > 0 then f [x] ! 0 > 0 else f [x] ! 0 <= 0

@property
elseExtends : Bool
elseExtends = forall x . 0 < x => if x > 1 then f [x] ! 0 > 0 else f [x] ! 0 <= 0 and f [x] ! 0 >= -1
