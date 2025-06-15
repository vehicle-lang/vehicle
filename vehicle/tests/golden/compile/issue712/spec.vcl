@network
f : Tensor Rat [1] -> Tensor Rat [1]

decide : Tensor Rat [1] -> Index 2
decide x = if f x ! 0 < 0.5 then 0 else 1

@property
isMalicious : Bool
isMalicious = decide [0] == 1
