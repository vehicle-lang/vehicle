-- Integers

int : Int
int = 0

addInt : Int
addInt = 3 + 2

subInt : Int
subInt = 3 - 2

multInt : Int
multInt = 3 * 2

negInt : Int
negInt = - 2

eqInt : Prop
eqInt = 0 == (1 : Int)

neqInt : Prop
neqInt = 0 != (1 : Int)

leqInt : Prop
leqInt = 0 <= (1 : Int)

ltInt : Prop
ltInt = 0 < (1 : Int)

geqInt : Prop
geqInt = 0 >= (1 : Int)

gtInt : Prop
gtInt = 0 > (1 : Int)
