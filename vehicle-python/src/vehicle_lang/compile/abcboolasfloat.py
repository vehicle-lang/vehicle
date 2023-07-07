from abc import ABCMeta
from dataclasses import dataclass
from typing import SupportsFloat

from typing_extensions import TypeVar, override

from ..ast import Builtin
from . import _numeric
from ._functools import Relation2
from .abcnumeric import ABCNumericBuiltins

_SupportsNat = TypeVar("_SupportsNat", bound=_numeric.SupportsNat)
_SupportsInt = TypeVar("_SupportsInt", bound=_numeric.SupportsInt)
_SupportsRat = TypeVar("_SupportsRat", bound=_numeric.SupportsRat)


@dataclass(frozen=True, init=False)
class ABCBoolAsFloatBuiltins(
    ABCNumericBuiltins[_SupportsRat, _SupportsNat, _SupportsInt, _SupportsRat],
    metaclass=ABCMeta,
):
    @override
    def EqIndex(self) -> Relation2[int, _SupportsRat]:
        return lambda x: lambda y: self.EqRat()(self.RatFromInt(self.Int(x)))(
            self.RatFromInt(self.Int(y))
        )

    @override
    def EqInt(self) -> Relation2[_SupportsInt, _SupportsRat]:
        return lambda x: lambda y: self.EqRat()(self.RatFromInt(x))(self.RatFromInt(y))

    @override
    def EqNat(self) -> Relation2[_SupportsNat, _SupportsRat]:
        return lambda x: lambda y: self.EqRat()(self.RatFromNat(x))(self.RatFromNat(y))

    @override
    def LtIndex(self) -> Relation2[int, _SupportsRat]:
        return lambda x: lambda y: self.LtRat()(self.Rat(x))(self.Rat(x))

    @override
    def LtInt(self) -> Relation2[_SupportsInt, _SupportsRat]:
        return lambda x: lambda y: self.LtRat()(self.RatFromInt(x))(self.RatFromInt(y))

    @override
    def LtNat(self) -> Relation2[_SupportsNat, _SupportsRat]:
        return lambda x: lambda y: self.LtRat()(self.RatFromNat(x))(self.RatFromNat(y))
