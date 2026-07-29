record TwoReal where
  { a : Real
  , b : Real
  }


p1 : TwoReal
p1 = { a = 3, b = 4 }

p2 : TwoReal
p2 = { a = 5, b = 12 }

test : Bool
test = (ltTC p1 p2)
