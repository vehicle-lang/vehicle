import operator
from functools import partial
from typing import Sequence, Type

from typing_extensions import TypeVar, override

from . import Builtins
from ._functools import (
    Function1,
    Function2,
    Function3,
    Operator1,
    Operator2,
    Relation2,
    curry,
)

_S = TypeVar("_S")
_T = TypeVar("_T")


class PythonBuiltins(
    Builtins[
        bool,
        int,
        int,
        int,
        float,
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
    def And(self) -> Operator2[bool]:
        return curry(operator.and_)

    @override
    def AtVector(self) -> Function2[Sequence[_T], int, _T]:
        return curry(operator.getitem)

    @override
    def Bool(self, value: bool) -> bool:
        return value

    @override
    def BoolType(self) -> Type[bool]:
        return bool

    @override
    def DivRat(self) -> Operator2[float]:
        return curry(operator.truediv)

    @override
    def Eq(self) -> Relation2[_T, bool]:
        return curry(operator.eq)

    @override
    def Exists(self) -> Function1[Function1[_T, bool], bool]:
        return NotImplemented

    @override
    def Forall(self) -> Function1[Function1[_T, bool], bool]:
        return NotImplemented

    @override
    def GeIndex(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def GeInt(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def GeNat(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def GeRat(self) -> Relation2[float, bool]:
        return curry(operator.ge)

    @override
    def GtIndex(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def GtInt(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def GtNat(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def GtRat(self) -> Relation2[float, bool]:
        return curry(operator.gt)

    @override
    def If(self) -> Function3[bool, _T, _T, _T]:
        return lambda i: lambda t: lambda e: t if i else e

    @override
    def Implies(self) -> Operator2[bool]:
        return lambda x: lambda y: (not x) or y

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
    def LeIndex(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def LeInt(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def LeNat(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def LeRat(self) -> Relation2[float, bool]:
        return curry(operator.le)

    @override
    def LtIndex(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def LtInt(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def LtNat(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def LtRat(self) -> Relation2[float, bool]:
        return curry(operator.lt)

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
    def Ne(self) -> Relation2[_T, bool]:
        return curry(operator.ne)

    @override
    def NegInt(self) -> Operator1[int]:
        return operator.neg

    @override
    def NegRat(self) -> Operator1[float]:
        return operator.neg

    @override
    def Not(self) -> Operator1[bool]:
        return operator.not_

    @override
    def Or(self) -> Operator2[bool]:
        return curry(operator.or_)

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
