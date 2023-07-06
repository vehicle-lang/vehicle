precedence : Rat -> Rat -> Rat -> Rat
precedence x y z = x + 2 * y - z / y

natLitNatLitDiv : Rat
natLitNatLitDiv = 1 / 2

@network
f : Vector Rat 1 -> Vector Rat 1

@property
property : Bool
property = f [natLitNatLitDiv] ! 0 >= 0
