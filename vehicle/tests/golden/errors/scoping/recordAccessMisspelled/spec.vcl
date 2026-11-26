record Test where
  { speed : Real
  }

example : Test
example = { speed = 2 }

test : Real
test = example.sped
