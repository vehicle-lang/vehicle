-- Reals

real : Real
real = 0.0

addReal : Real
addReal = 3.0 + 2.0

subReal : Real
subReal = 3.0 - 2.0

multReal : Real
multReal = 3.0 * 2.0

divReal : Real
divReal = 3.0 / 2.0

negReal : Real
negReal = - 2.0

eqReal : Prop
eqReal = 0.0 == (1.0 : Real)

neqReal : Prop
neqReal = 0.0 != (1.0 : Real)

leqReal : Prop
leqReal = 0.0 <= (1.0 : Real)

ltReal : Prop
ltReal = 0.0 < (1.0 : Real)

geqReal : Prop
geqReal = 0.0 >= (1.0 : Real)

gtReal : Prop
gtReal = 0.0 > (1.0 : Real)
