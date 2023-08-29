@network
f : Vector Rat 1 -> Vector Rat 1

decide : Vector Rat 1 -> Index 2
decide x = if f x ! 0 < 0.5 then 0 else 1

@property
isMalcious : Bool
isMalcious = decide [0] == 1
