import operator
from abc import ABCMeta
from dataclasses import dataclass
from typing import Any, Callable, Dict, Iterator

from typing_extensions import TypeAlias, TypeVar, override

from . import _numeric
from ._functools import Function2, Operator1, Operator2, curry
from .abc import Builtins

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

_Bool = TypeVar("_Bool")
_SupportsNat = TypeVar("_SupportsNat", bound=_numeric.SupportsNat)
_SupportsInt = TypeVar("_SupportsInt", bound=_numeric.SupportsInt)
_SupportsRat = TypeVar("_SupportsRat", bound=_numeric.SupportsRat)
_Any = TypeVar("_Any")

Sampler: TypeAlias = Callable[[Dict[str, Any]], Iterator[Any]]


@dataclass(frozen=True)
class ABCNumericBuiltins(
    Builtins[
        _Bool,
        _SupportsNat,
        _SupportsInt,
        _SupportsRat,
        _Any,
    ],
    metaclass=ABCMeta,
):
    @override
    def AddInt(self) -> Operator2[_SupportsInt]:
        return curry(operator.add)

    @override
    def AddNat(self) -> Operator2[_SupportsNat]:
        return curry(operator.add)

    @override
    def AddRat(self) -> Operator2[_SupportsRat]:
        return curry(operator.add)

    @override
    def DivRat(self) -> Operator2[_SupportsRat]:
        return curry(operator.truediv)

    def IntFromNat(self, value: _SupportsNat) -> _SupportsInt:
        return self.Int(value.__int__())

    @override
    def MaxRat(self) -> Operator2[_SupportsRat]:
        return curry(max)

    @override
    def MinRat(self) -> Operator2[_SupportsRat]:
        return curry(min)

    @override
    def MulInt(self) -> Operator2[_SupportsInt]:
        return curry(operator.mul)

    @override
    def MulNat(self) -> Operator2[_SupportsNat]:
        return curry(operator.mul)

    @override
    def MulRat(self) -> Operator2[_SupportsRat]:
        return curry(operator.mul)

    @override
    def NegInt(self) -> Operator1[_SupportsInt]:
        return lambda x: -x

    @override
    def NegRat(self) -> Operator1[_SupportsRat]:
        return lambda x: -x

    @override
    def PowRat(self) -> Function2[_SupportsRat, _SupportsInt, _SupportsRat]:
        return lambda x: lambda y: x ** y.__int__()

    def RatFromNat(self, value: _SupportsNat) -> _SupportsRat:
        return self.Rat(value.__int__())

    def RatFromInt(self, value: _SupportsInt) -> _SupportsRat:
        return self.Rat(value.__int__())

    @override
    def SubInt(self) -> Operator2[_SupportsInt]:
        return curry(operator.sub)

    @override
    def SubRat(self) -> Operator2[_SupportsRat]:
        return curry(operator.sub)
