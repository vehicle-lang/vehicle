--------------------------------------------------------------------------------
-- Inputs
inputSize = 30
type InputVector = Tensor Rat [inputSize]
--------------------------------------------------------------------------------
-- Outputs
type OutputVector = Tensor Rat [2]
type Label = Index 2
pos : Label
pos = 0
neg : Label
neg = 1
--------------------------------------------------------------------------------
-- Network
@network
classifier : InputVector -> OutputVector
advises : InputVector -> Label -> Bool
advises x i = forall j . j != i => classifier x ! i > classifier x ! j
--------------------------------------------------------------------------------
-- Dataset
@parameter(infer=True)
n : Nat

@dataset
inputs : Tensor Rat [n, inputSize]

minList : Tensor Rat [n] -> Rat
minList v = reduceMin 1 v

maxList : Tensor Rat [n] -> Rat
maxList v = reduceMax 0 v

inputTranspose : Tensor Rat [inputSize, n]
inputTranspose = foreach i . foreach j . inputs ! j ! i

vectorMin : InputVector
vectorMin = foreach i . minList (inputTranspose ! i)

vectorMax :  InputVector
vectorMax = foreach i . maxList (inputTranspose ! i)

hyperRectangle : InputVector -> Bool
hyperRectangle x = forall i . vectorMin ! i  <= x ! i <= vectorMax ! i

@property
property : Bool
property = forall x . hyperRectangle x => advises x pos
