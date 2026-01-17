-- Check addition and subtraction typecheck for tensorLikes

@tensor
record Test1 where
  { f1 : Real
  , f2 : Real
  }

value1 : Test1
value1 = { f1 = 1, f2 = 2 }

value2 : Test1
value2 = { f1 = 1, f2 = 2 }

adding : Test1
adding = value1 + value2

subtracting : Test1
subtracting = value1 - value2
