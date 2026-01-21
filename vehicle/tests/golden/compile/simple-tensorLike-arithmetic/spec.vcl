@tensor
record Test1 where
  { f1 : Real
  , f2 : Real
  }

value1 : Test1
value1 = { f1 = 1, f2 = 2 }

value2 : Test1
value2 = { f1 = 1, f2 = 2 }

add : Test1
add = value1 + value2

subtract : Test1
subtract = value1 - value2
