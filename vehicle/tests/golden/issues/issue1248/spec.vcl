@network
classifier : Tensor Real [1] -> Tensor Real [1]

@property
robustAround : Bool
robustAround = forall (delta : Tensor Real [1]) . forall j . 0 >= classifier (foreach k . delta ! k) ! j
