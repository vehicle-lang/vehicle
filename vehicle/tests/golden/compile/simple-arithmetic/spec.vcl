precedence : Real -> Real -> Real -> Real
precedence x y z = x + 2 * y - z / y

natLitNatLitDiv : Real
natLitNatLitDiv = 1 / 2

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
property : Bool
property = f [natLitNatLitDiv] ! 0 >= 0
