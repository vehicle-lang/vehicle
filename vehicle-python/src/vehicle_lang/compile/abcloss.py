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


@dataclass(frozen=True)
class UnsupportedBuiltin(Exception):
    builtin: Builtin


@dataclass(frozen=True, init=False)
class ABCLossBuiltins(
    ABCNumericBuiltins[
        _SupportsRat, _SupportsNat, _SupportsInt, _SupportsRat, SupportsFloat
    ],
    metaclass=ABCMeta,
):
    @override
    def LtIndex(self) -> Relation2[int, _SupportsRat]:
        return lambda x: lambda y: self.LtRat()(self.Rat(x))(self.Rat(x))

    @override
    def LtInt(self) -> Relation2[_SupportsInt, _SupportsRat]:
        return lambda x: lambda y: self.LtRat()(self.RatFromInt(x))(self.RatFromInt(y))

    @override
    def LtNat(self) -> Relation2[_SupportsNat, _SupportsRat]:
        return lambda x: lambda y: self.LtRat()(self.RatFromNat(x))(self.RatFromNat(y))
