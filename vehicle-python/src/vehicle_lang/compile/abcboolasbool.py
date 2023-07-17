from abc import ABCMeta
from dataclasses import dataclass

from typing_extensions import TypeVar, override

from . import _numeric
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
    @override
    def And(self, x: bool, y: bool) -> bool:
        assert isinstance(x, bool), f"Expected bool, found {x}"
        assert isinstance(y, bool), f"Expected bool, found {y}"
        return x and y

    @override
    def Bool(self, value: bool) -> bool:
        assert isinstance(value, bool), f"Expected bool, found {value}"
        return bool(value)

    @override
    def EqIndex(self, x: int, y: int) -> bool:
        assert isinstance(x, int), f"Expected int, found {x}"
        assert isinstance(y, int), f"Expected int, found {y}"
        return x == y

    @override
    def EqInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x == y

    @override
    def EqNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x == y

    @override
    def EqRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x == y

    @override
    def If(self, cond: bool, if_true: _T, if_false: _T) -> _T:
        assert isinstance(cond, bool), f"Expected bool, found {cond}"
        return if_true if cond else if_false

    @override
    def LtIndex(self, x: int, y: int) -> bool:
        assert isinstance(x, int), f"Expected int, found {x}"
        assert isinstance(y, int), f"Expected int, found {y}"
        return x < y

    @override
    def LtInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x < y

    @override
    def LtNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x < y

    @override
    def LtRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x < y

    @override
    def Not(self, x: bool) -> bool:
        assert isinstance(x, bool), f"Expected bool, found {x}"
        return not x

    @override
    def Or(self, x: bool, y: bool) -> bool:
        assert isinstance(x, bool), f"Expected bool, found {x}"
        assert isinstance(y, bool), f"Expected bool, found {y}"
        return x or y
