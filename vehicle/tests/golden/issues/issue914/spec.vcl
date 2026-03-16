f : Tensor Real [1, 1]
f = [[0]]

-- Type error, `reduceMin` should have an extra argument.
vectorMin : Tensor Real [1]
vectorMin = foreach i . reduceMin (f ! i)
