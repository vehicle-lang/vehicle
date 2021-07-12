-- Miscallaenous

fun : Real -> Real
fun x = x


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
eqReal = 0.0 == 1.0

neqReal : Prop
neqReal = 0.0 != 1.0

leqReal : Prop
leqReal = 0.0 <= 1.0

ltReal : Prop
ltReal = 0.0 < 1.0

geqReal : Real
geqReal = 0.0 >= 1.0

gtReal : Real
gtReal = 0.0 > 1.0

tensorReal : Tensor Real [2]
tensorReal = [0.0, 1.0]


-- Integers

int : Int
int = 0

-- addInt : Int
-- addInt = 3 + 2

-- subInt : Int
-- subInt = 3 - 2

-- multInt : Int
-- multInt = 3 * 2

-- divInt : Int
-- divInt = 3 / 2

-- negInt : Int
-- negInt = - 2

-- eqInt : Prop
-- eqInt = 0 == 1

-- neqInt : Prop
-- neqInt = 0.0 == 1.0

-- leqInt : Prop
-- leqInt = 0 <= 1

-- ltInt : Prop
-- ltInt = 0 < 1

-- geqInt : Prop
-- geqInt = 0 >= 1

-- gtInt : Prop
-- gtInt = 0 > 1

tensorInt : Tensor Int [2]
tensorInt = [0, 1]