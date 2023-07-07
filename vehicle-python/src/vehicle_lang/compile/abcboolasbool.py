import operator
from abc import ABCMeta
from dataclasses import dataclass
from typing import Any, SupportsFloat, SupportsInt

from typing_extensions import TypeVar, final, override

from . import _numeric
from ._functools import Function3, Operator1, Operator2, Relation2, curry
from .abcnumeric import ABCNumericBuiltins

_SupportsNat = TypeVar("_SupportsNat", bound=_numeric.SupportsNat)
_SupportsInt = TypeVar("_SupportsInt", bound=_numeric.SupportsInt)
_SupportsRat = TypeVar("_SupportsRat", bound=_numeric.SupportsRat)

_T = TypeVar("_T")


@dataclass(frozen=True)
class ABCBoolAsBoolBuiltins(
    ABCNumericBuiltins[
        bool,
        _SupportsNat,
        _SupportsInt,
        _SupportsRat,
    ],
    metaclass=ABCMeta,
):
    @final
    @override
    def And(self) -> Operator2[bool]:
        return lambda x: lambda y: x and y

    @final
    @override
    def Bool(self, value: bool) -> bool:
        return value.__bool__()

    @final
    @override
    def EqIndex(self) -> Relation2[int, bool]:
        return curry(operator.eq)

    @final
    @override
    def EqInt(self) -> Relation2[_SupportsInt, bool]:
        return curry(operator.eq)

    @final
    @override
    def EqNat(self) -> Relation2[_SupportsNat, bool]:
        return curry(operator.eq)

    @final
    @override
    def EqRat(self) -> Relation2[_SupportsRat, bool]:
        return curry(operator.eq)

    @final
    @override
    def GeIndex(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @final
    @override
    def GeInt(self) -> Relation2[_SupportsInt, bool]:
        return curry(operator.ge)

    @final
    @override
    def GeNat(self) -> Relation2[_SupportsNat, bool]:
        return curry(operator.ge)

    @final
    @override
    def GeRat(self) -> Relation2[_SupportsRat, bool]:
        return curry(operator.ge)

    @final
    @override
    def GtIndex(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @final
    @override
    def GtInt(self) -> Relation2[_SupportsInt, bool]:
        return curry(operator.gt)

    @final
    @override
    def GtNat(self) -> Relation2[_SupportsNat, bool]:
        return curry(operator.gt)

    @final
    @override
    def GtRat(self) -> Relation2[_SupportsRat, bool]:
        return curry(operator.gt)

    @final
    @override
    def If(self) -> Function3[bool, _T, _T, _T]:
        return lambda i: lambda t: lambda e: t if i else e

    @final
    @override
    def Implies(self) -> Operator2[bool]:
        return lambda x: lambda y: (not x) or y

    @final
    @override
    def LeIndex(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @final
    @override
    def LeInt(self) -> Relation2[_SupportsInt, bool]:
        return curry(operator.le)

    @final
    @override
    def LeNat(self) -> Relation2[_SupportsNat, bool]:
        return curry(operator.le)

    @final
    @override
    def LeRat(self) -> Relation2[_SupportsRat, bool]:
        return curry(operator.le)

    @final
    @override
    def LtIndex(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @final
    @override
    def LtInt(self) -> Relation2[_SupportsInt, bool]:
        return curry(operator.lt)

    @final
    @override
    def LtNat(self) -> Relation2[_SupportsNat, bool]:
        return curry(operator.lt)

    @final
    @override
    def LtRat(self) -> Relation2[_SupportsRat, bool]:
        return curry(operator.lt)

    @final
    @override
    def Not(self) -> Operator1[bool]:
        return operator.not_

    @final
    @override
    def Or(self) -> Operator2[bool]:
        return lambda x: lambda y: x or y
