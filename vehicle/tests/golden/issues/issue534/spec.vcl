-- The `else` branch of an `if` may contain another `if` (or a quantifier,
-- `let` or lambda) without brackets, so `if`s can be chained, including
-- across multiple lines.

maxValues : Vector Real 20
maxValues = foreach i .
  if i == 5 then 100.0 else
  if i == 6 then 96.5 else
  if i == 17 then 101.2 else
  0

singleLine : Index 3 -> Nat
singleLine i = if i == 0 then 1 else if i == 1 then 2 else 3

@property
quantifierInElse : Bool
quantifierInElse = if 1 >= 0 then True else forall (x : Index 2) . x >= 0

@property
letInElse : Bool
letInElse = if 1 >= 0 then True else let y = 2 in y >= (0 : Nat)
