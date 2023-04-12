precedence : Rat -> Rat -> Rat -> Rat
precedence x y z = x + 2 * y - z / y

natLitNatLitDiv : Rat
natLitNatLitDiv = 1 / 2

@network
f : Vector Rat 1 -> Vector Rat 1

natNatDiv : Nat -> Nat -> Rat
natNatDiv x y = f [ x / y ] ! 0

@property
property : Bool
property = natNatDiv 1 2 >= 0
