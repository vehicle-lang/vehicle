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