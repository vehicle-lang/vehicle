--------------------------------------------------------------------------------
-- Inputs
inputSize = 30
type InputVector = Tensor Rat [inputSize]
--------------------------------------------------------------------------------
-- Outputs
type OutputVector = Vector Rat 2
type Label = Index 2
pos = 0
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

minList : Vector Rat n -> Rat
minList v = fold min 0 v

maxList : Vector Rat n -> Rat
maxList v = fold max 1 v

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
property = forall x . hyperRectangle x =>  advises x pos
