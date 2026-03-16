@network
f : Tensor Real [1] -> Tensor Real [1]

@dataset
trainingDataset : Tensor Real [if f [0] ! 0 > 0 then 2 else 3]
