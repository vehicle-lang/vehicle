import operator
from functools import partial
from typing import Sequence, Type, Union

from typing_extensions import TypeAlias, TypeVar, override

from ._functools import (
    Function1,
    Function2,
    Function3,
    Operator1,
    Operator2,
    Relation2,
    curry,
)
from .abc import Builtins

Number: TypeAlias = Union[int, float]

_T = TypeVar("_T")


class ABCLossBuiltins(
    Builtins[
        float,
        int,
        int,
        int,
        float,
        Number,
    ],
):
    @override
    def AddInt(self) -> Operator2[int]:
        return curry(operator.add)

    @override
    def AddNat(self) -> Operator2[int]:
        return curry(operator.add)

    @override
    def AddRat(self) -> Operator2[float]:
        return curry(operator.add)

    @override
    def AtVector(self) -> Function2[Sequence[_T], int, _T]:
        return curry(operator.getitem)

    @override
    def BoolType(self) -> Type[float]:
        return float

    @override
    def DivRat(self) -> Operator2[float]:
        return curry(operator.truediv)

    @override
    def If(self) -> Function3[float, _T, _T, _T]:
        return NotImplemented

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def IndexType(self) -> Type[int]:
        return int

    @override
    def Indices(self) -> Function1[int, Sequence[int]]:
        return partial(range, 0)

    @override
    def Int(self, value: int) -> int:
        return value

    @override
    def IntType(self) -> Type[int]:
        return int

    @override
    def GeIndex(self) -> Relation2[int, float]:
        return self.GeInt()

    @override
    def GeInt(self) -> Relation2[int, float]:
        return curry(operator.ge)

    @override
    def GeNat(self) -> Relation2[int, float]:
        return self.GeInt()

    @override
    def GeRat(self) -> Relation2[float, float]:
        return curry(operator.ge)

    @override
    def GtIndex(self) -> Relation2[int, float]:
        return curry(operator.gt)

    @override
    def GtInt(self) -> Relation2[int, float]:
        return curry(operator.gt)

    @override
    def GtNat(self) -> Relation2[int, float]:
        return curry(operator.gt)

    @override
    def GtRat(self) -> Relation2[float, float]:
        return curry(operator.gt)

    @override
    def LeIndex(self) -> Relation2[int, float]:
        return self.LeInt()

    @override
    def LeNat(self) -> Relation2[int, float]:
        return self.LeInt()

    @override
    def LtIndex(self) -> Relation2[int, float]:
        return self.LtInt()

    @override
    def LtInt(self) -> Relation2[int, float]:
        ...

    @override
    def LtNat(self) -> Relation2[int, float]:
        return self.LtInt()

    @override
    def LtRat(self) -> Relation2[float, float]:
        ...

    @override
    def MaxRat(self) -> Operator2[float]:
        return curry(max)

    @override
    def MinRat(self) -> Operator2[float]:
        return curry(min)

    @override
    def MulInt(self) -> Operator2[int]:
        return curry(operator.mul)

    @override
    def MulNat(self) -> Operator2[int]:
        return curry(operator.mul)

    @override
    def MulRat(self) -> Operator2[float]:
        return curry(operator.mul)

    @override
    def Nat(self, value: int) -> int:
        return value

    @override
    def NatType(self) -> Type[int]:
        return int

    @override
    def NegInt(self) -> Operator1[int]:
        return operator.neg

    @override
    def NegRat(self) -> Operator1[float]:
        return operator.neg

    @override
    def PowRat(self) -> Operator2[float]:
        return curry(operator.pow)

    @override
    def Rat(self, numerator: int, denominator: int) -> float:
        return numerator / denominator

    @override
    def RatType(self) -> Type[float]:
        return float

    @override
    def SubInt(self) -> Operator2[int]:
        return curry(operator.sub)

    @override
    def SubRat(self) -> Operator2[float]:
        return curry(operator.sub)
