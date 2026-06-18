@tensor
record Typ1 where {
    a1 : Real,
    b1 : Real
}

@tensor
record Typ2 where {
    a2 : Real,
    b2 : Real
}

@tensor
record Typ3 where {
    a3 : Real,
    b3 : Real
}

@tensor
record Typ4 where {
    a4 : Real,
    b4 : Real
}

typ1Val1 : Typ1
typ1Val1 = {a1 = 1, b1 = 1}

typ1Val2 : Typ1
typ1Val2 = {a1 = 2, b1 = 2 }

typ1Sum : Typ1
typ1Sum = typ1Val1 + typ1Val2


typ2Val1 : Typ2
typ2Val1 = {a2 = 1, b2 = 1}

typ2Val2 : Typ2
typ2Val2 = {a2 = 2, b2 = 2}

typ2Sum : Typ2
typ2Sum = typ2Val1 + typ2Val2


typ3Val1 : Typ3
typ3Val1 = {a3 = 1, b3 = 1}

typ3Val2 : Typ3
typ3Val2 = {a3 = 2, b3 = 2}

typ3Sum : Typ3
typ3Sum = typ3Val1 + typ3Val2


typ4Val1 : Typ4
typ4Val1 = {a4 = 1, b4 = 1}

typ4Val2 : Typ4
typ4Val2 = {a4 = 2, b4 = 2}

typ4Sum : Typ4
typ4Sum = typ4Val1 + typ4Val2