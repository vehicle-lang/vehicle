@network
f : Tensor Real [1] -> Tensor Real [1]

decide : Tensor Real [1] -> Index 2
decide x = if f x ! 0 < 0.5 then 0 else 1

@property
isMalicious : Bool
isMalicious = decide [0] == 1
