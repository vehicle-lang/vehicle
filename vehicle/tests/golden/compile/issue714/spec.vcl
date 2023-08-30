type InputVector = Vector Rat 50
type OutputVector = Vector Rat 1
type NormalisedInputVector = Vector Rat 50

type Label = Index 2

malicious = 1
benign = 0

maxValues : InputVector
maxValues =
  [742375299,
   71891,
   142246,
   52230856,
   196028307,
     4380,
     1460,
     1398,
    18980,
     2920,
     4305,
    693139418,
    427628329,
    608186719,
    608186719,
    742321552,
    428536994,
    742272415,
    607777412,
        1,
        0,
        0,
        0,
    1439748,
    2844928,
        2,
       24,
       68,
    18829,
    214136,
        0,
        2,
        2,
       28,
     2470,
     4439,
        0,
        0,
        0,
    113001890,
    77399,
    30432989,
        1,
     1455,
        0,
     1205,
    65535,
    65535,
    37896,
       44]

minValues : InputVector
minValues =
  [
  0,
  1,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
  ]

normalise : InputVector -> NormalisedInputVector
normalise x = foreach i .
  let max = maxValues ! i in
  let min = minValues ! i in
  if max == min
    then x ! i
    else (x ! i - min) / (max - min)

-------------
-- Network --
-------------

@network
classify : NormalisedInputVector -> OutputVector

-------------
-- Dataset --
-------------

-- @parameter(infer=True)
datasetSize : Nat
datasetSize = 8

@dataset
inputDataset : Vector InputVector datasetSize

@property
isSane : Bool
isSane = classify (normalise (inputDataset ! 0)) ! 0 <= 10
