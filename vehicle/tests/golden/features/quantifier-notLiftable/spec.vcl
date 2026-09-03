@network
f: Real -> Real


@property
working: Bool
working = (exists (x1 : Real) . 0 < x1 < 1) and (exists (x2 : Real) . 1 < x2 < 2)

-- exists lifted to top level
-- x1 missing lower and upper bounds error
@property
notWorking1: Bool
notWorking1 = (exists (x1 : Real) . 0 < x1 < 1) or (exists (x2 : Real) . 1 < x2 < 2)

-- forall not lifted
-- x1 missing upper bounds error
@property
notWorking: Bool
notWorking = (forall (x1 : Real) . 0 < x1 < 1) or (forall (x2 : Real) . 1 < x2 < 2)

@property
liftable: Bool
liftable = (exists (x1 : Real) . (0 < x1 < 1 and (exists (x2 : Real) . 0 < x2 < x1))) and (exists (x3 : Real) . 1 < x3 < 2)

-- All quantifiers lifted to top level
-- x2 missing lower and upper bounds error
-- @property
-- liftable: Bool
-- liftable = forall (x1 : Real) . (0 < x1 < 1 and (forall (x2 : Real) . 0 < x2 < x1))

-- Should not throw error (quantifiers are not alternating)
-- @property
-- notLiftable1: Bool
-- notLiftable1 = (forall (x1 : Real) . 0 < x1 < 1 or x1 >= 2) and (exists (x2 : Real) . 1 < x2 < 2 and x2 <= 3)

-- Should throw error (quantifiers are alternating)
@property
notLiftable2: Bool
notLiftable2 = forall (x1 : Real) . (0 < x1 < 1 and (exists (x2 : Real) . x2 < x1))

-- exists should be lifted to top of RHS argument
-- @property
-- notLiftable3: Bool
-- notLiftable3 = (exists (x1 : Real) . 0 < x1 < 1) and ((exists (x2 : Real) . 1 < x2 < 2) or (exists (x3 : Real) . 2 < x3 < 3))

-- forall should be lifted to top of LHS argument
-- @property
-- notLiftable4: Bool
-- notLiftable4 = ((forall (x1 : Real) . 0 < x1 < 1 or x1 > 2) and (forall (x2 : Real) . 1 < x2 < 2 or x2 > 3)) or (forall (x1 : Real) . 0 < x1 < 1 or x1 > 2)

-- forall should be lowered into both arguments
-- @property
-- notLiftable5: Bool
-- notLiftable5 = (forall (x1 : Real) . 0 < x1 < 1 or (forall (x2 : Real) . x1 < x2))

-- This produces error: "Unblocking evaluation results in unevaluable result"
-- @property
-- notLiftable6: Bool
-- notLiftable6 = (forall x1 . 0 < x1 < 1 => f x1 >= 1) or ((forall x2 . 1 < x2 < 2 => f x2 >= 2) and (forall x3 . 2 < x3 < 3 => f x3 >= 3))
