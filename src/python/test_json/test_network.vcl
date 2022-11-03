@network
net : Tensor Rat [1] -> Tensor Rat [1]

@property
net_prop : Bool
net_prop = net [1] ! 0 >= 0
