from dataclasses import dataclass
from typing import SupportsFloat, SupportsInt

from typing_extensions import TypeVar, final, override

from ..ast import If
from . import _numeric
from ._functools import Function3, Operator1, Operator2, Relation2
from .abc import UnsupportedBuiltin
from .abcboolasfloat import ABCBoolAsFloatBuiltins

_SupportsNat = TypeVar("_SupportsNat", bound=_numeric.SupportsNat)
_SupportsInt = TypeVar("_SupportsInt", bound=_numeric.SupportsInt)
_SupportsRat = TypeVar("_SupportsRat", bound=_numeric.SupportsRat)
_T = TypeVar("_T")


@dataclass(frozen=True)
class ABCLossGodelBuiltins(
    ABCBoolAsFloatBuiltins[_SupportsRat, _SupportsNat, _SupportsInt]
):
    @override
    def And(self) -> Operator2[_SupportsRat]:
        return self.MinRat()

    @override
    def Bool(self, value: bool) -> _SupportsRat:
        return self.Rat(1) if value else self.Rat(0)

    @override
    def EqRat(self) -> Relation2[_SupportsRat, _SupportsRat]:
        return lambda x: lambda y: self.Rat(1) - (x - y / x + y).__abs__()

    @override
    def If(self) -> Function3[_SupportsRat, _T, _T, _T]:
        raise UnsupportedBuiltin(builtin=If())

    @override
    def LtInt(self) -> Relation2[_SupportsInt, _SupportsRat]:
        return lambda x: lambda y: self.LtRat()(self.RatFromInt(x))(self.RatFromInt(y))

    @override
    def LtRat(self) -> Relation2[_SupportsRat, _SupportsRat]:
        return lambda x: lambda y: self.Rat(1) - self.MaxRat()(x - y / x + y)(
            self.Rat(0)
        )

    @override
    def Not(self) -> Operator1[_SupportsRat]:
        return self.NegRat()

    @override
    def Or(self) -> Operator2[_SupportsRat]:
        return self.MaxRat()


@final
@dataclass(frozen=True)
class PythonLossGodelBuiltins(ABCLossGodelBuiltins[float, int, int]):
    @final
    @override
    def Int(self, value: SupportsInt) -> int:
        return value.__int__()

    @final
    @override
    def Nat(self, value: SupportsInt) -> int:
        return value.__int__()

    @final
    @override
    def Rat(self, value: SupportsFloat) -> float:
        return value.__float__()
