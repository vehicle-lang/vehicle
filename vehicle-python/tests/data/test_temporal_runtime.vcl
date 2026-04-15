@network
network : Tensor Real [4] -> Tensor Real [4]

@property
prop_globally : Bool
prop_globally = (globally[0,1] (network [1, 2, 3, 4] >. [0, 0, 0, 0])) ! 0

@property
prop_finally : Bool
prop_finally = (finally[0,2] (network [1, 2, 3, 4] >. [0, 0, 0, 0])) ! 0

@property
prop_until : Bool
prop_until = (until[0,2] (network [1, 2, 3, 4] >. [0, 0, 0, 0]) (network [4, 3, 2, 1] >. [0, 0, 0, 0])) ! 0
