fun : Real -> Real
fun x = x

real : Real
real = 0.0

int : Int
int = 0

leq : Real -> Prop
leq x = x <= 0.0

lt : Real -> Prop
lt x = x < 0.0

geq : Real -> Prop
geq x = x >= 0.0

gt : Real -> Prop
gt x = x > 0.0

tensor : Tensor Real [2]
tensor = [0.0, 1.0]

addReal : Real
addReal = 3.0 + 2.0

subReal : Real
subReal = 3.0 - 2.0

multReal : Real
multReal = 3.0 * 2.0

divReal : Real
divReal = 3.0 / 2.0


