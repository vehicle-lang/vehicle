@network
f : Tensor Real [4] -> Tensor Real [4]

@property
boundedSig : Bool
boundedSig = (until[0,3]
                (f [0.0, 0.0, 0.0, 0.0] >=. const 0.0 [4])
                (f [0.0, 0.0, 0.0, 0.0] >=. const 1.0 [4])) ! 0
