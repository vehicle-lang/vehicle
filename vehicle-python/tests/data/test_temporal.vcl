import STL

@network
network : Tensor Real [4] -> Tensor Real [4]

phi : Tensor Bool [4]
phi = network [1, 2, 3, 4] >. [0, 0, 0, 0]

psi : Tensor Bool [4]
psi = network [4, 3, 2, 1] >. [0, 0, 0, 0]

@property
prop_globally : Bool
prop_globally = (globally [0,1] phi) ! 0

@property
prop_finally : Bool
prop_finally = (finally [0,2] phi) ! 0

@property
prop_until : Bool
prop_until = (until [0,2] phi psi) ! 0
