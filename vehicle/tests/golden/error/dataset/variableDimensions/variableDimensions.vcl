@network
f : Tensor Rat [1] -> Tensor Rat [1]

@dataset
trainingDataset : Tensor Rat [if f [0] ! 0 > 0 then 2 else 3]
