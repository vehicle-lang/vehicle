record Input where
  { position : Real
  -- , b : Real
  }


-- p1 : Input
-- p1 = { a = 3, b = 4 }
-- p1 = { position = 3 }

-- p2 : Input
-- p2 = { position = 3 }
-- p2 = { a = 5, b = 12 }

-- @network
-- model : Input -> Real

model : Input -> Real
model p = p.position + 1

derivative : Input -> Real
derivative = delta[model]/delta[position]

-- derivativeAtOne : Real
-- derivativeAtOne = delta[model]/delta[position] ({ position = 1 })
