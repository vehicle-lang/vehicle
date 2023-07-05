import operator
from functools import partial
from typing import Sequence, Type

from typing_extensions import TypeVar, override

from . import BuiltinInterpreter
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


class PythonBuiltinInterpreter(
    BuiltinInterpreter[
        bool,
        int,
        int,
        int,
        float,
    ],
):
    @override
    def interpret_AddInt(self) -> Operator2[int]:
        return curry(operator.add)

    @override
    def interpret_AddNat(self) -> Operator2[int]:
        return curry(operator.add)

    @override
    def interpret_AddRat(self) -> Operator2[float]:
        return curry(operator.add)

    @override
    def interpret_And(self) -> Operator2[bool]:
        return curry(operator.and_)

    @override
    def interpret_AtVector(self) -> Function2[Sequence[_T], int, _T]:
        return curry(operator.getitem)

    @override
    def interpret_Bool(self, value: bool) -> bool:
        return value

    @override
    def interpret_BoolType(self) -> Type[bool]:
        return bool

    @override
    def interpret_DivRat(self) -> Operator2[float]:
        return curry(operator.truediv)

    @override
    def interpret_Eq(self) -> Relation2[_T, bool]:
        return curry(operator.eq)

    @override
    def interpret_Exists(self) -> Function1[Function1[_T, bool], bool]:
        return NotImplemented

    @override
    def interpret_Forall(self) -> Function1[Function1[_T, bool], bool]:
        return NotImplemented

    @override
    def interpret_GeIndex(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def interpret_GeInt(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def interpret_GeNat(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def interpret_GeRat(self) -> Relation2[float, bool]:
        return curry(operator.ge)

    @override
    def interpret_GtIndex(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def interpret_GtInt(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def interpret_GtNat(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def interpret_GtRat(self) -> Relation2[float, bool]:
        return curry(operator.gt)

    @override
    def interpret_If(self) -> Function3[bool, _T, _T, _T]:
        return lambda i: lambda t: lambda e: t if i else e

    @override
    def interpret_Implies(self) -> Operator2[bool]:
        return lambda x: lambda y: (not x) or y

    @override
    def interpret_Index(self, value: int) -> int:
        return value

    @override
    def interpret_IndexType(self) -> Type[int]:
        return int

    @override
    def interpret_Indices(self) -> Function1[int, Sequence[int]]:
        return partial(range, 0)

    @override
    def interpret_Int(self, value: int) -> int:
        return value

    @override
    def interpret_IntType(self) -> Type[int]:
        return int

    @override
    def interpret_LeIndex(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def interpret_LeInt(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def interpret_LeNat(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def interpret_LeRat(self) -> Relation2[float, bool]:
        return curry(operator.le)

    @override
    def interpret_LtIndex(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def interpret_LtInt(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def interpret_LtNat(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def interpret_LtRat(self) -> Relation2[float, bool]:
        return curry(operator.lt)

    @override
    def interpret_MaxRat(self) -> Operator2[float]:
        return curry(max)

    @override
    def interpret_MinRat(self) -> Operator2[float]:
        return curry(min)

    @override
    def interpret_MulInt(self) -> Operator2[int]:
        return curry(operator.mul)

    @override
    def interpret_MulNat(self) -> Operator2[int]:
        return curry(operator.mul)

    @override
    def interpret_MulRat(self) -> Operator2[float]:
        return curry(operator.mul)

    @override
    def interpret_Nat(self, value: int) -> int:
        return value

    @override
    def interpret_NatType(self) -> Type[int]:
        return int

    @override
    def interpret_Ne(self) -> Relation2[_T, bool]:
        return curry(operator.ne)

    @override
    def interpret_NegInt(self) -> Operator1[int]:
        return operator.neg

    @override
    def interpret_NegRat(self) -> Operator1[float]:
        return operator.neg

    @override
    def interpret_Not(self) -> Operator1[bool]:
        return operator.not_

    @override
    def interpret_Or(self) -> Operator2[bool]:
        return curry(operator.or_)

    @override
    def interpret_PowRat(self) -> Operator2[float]:
        return curry(operator.pow)

    @override
    def interpret_Rat(self, numerator: int, denominator: int) -> float:
        return numerator / denominator

    @override
    def interpret_RatType(self) -> Type[float]:
        return float

    @override
    def interpret_SubInt(self) -> Operator2[int]:
        return curry(operator.sub)

    @override
    def interpret_SubRat(self) -> Operator2[float]:
        return curry(operator.sub)
