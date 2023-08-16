import operator
from abc import ABCMeta
from dataclasses import dataclass
from typing import Any, Callable, Dict, Iterator

from typing_extensions import TypeAlias, TypeVar, override

from vehicle_lang.typing import AbstractVariableDomain

from . import _numeric
from .abc import Builtins

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

_Bool = TypeVar("_Bool")
_SupportsNat = TypeVar("_SupportsNat", bound=_numeric.SupportsNat)
_SupportsInt = TypeVar("_SupportsInt", bound=_numeric.SupportsInt)
_SupportsRat = TypeVar("_SupportsRat", bound=_numeric.SupportsRat)
_Variable = TypeVar("_Variable")
_VariableValue = TypeVar("_VariableValue")

Sampler: TypeAlias = Callable[[Dict[str, Any]], Iterator[Any]]


@dataclass(frozen=True)
class ABCNumericBuiltins(
    Builtins[
        _Bool,
        _SupportsNat,
        _SupportsInt,
        _SupportsRat,
        _Variable,
        _VariableValue,
    ],
    metaclass=ABCMeta,
):
    @override
    def AddInt(self, x: _SupportsInt, y: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x + y

    @override
    def AddNat(self, x: _SupportsNat, y: _SupportsNat) -> _SupportsNat:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x + y

    @override
    def AddRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x + y

    @override
    def DivRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x / y

    @override
    def MaxRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return max(x, y)

    @override
    def MinRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return min(x, y)

    @override
    def MulInt(self, x: _SupportsInt, y: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x * y

    @override
    def MulNat(self, x: _SupportsNat, y: _SupportsNat) -> _SupportsNat:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x * y

    @override
    def MulRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x * y

    @override
    def NegInt(self, x: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        return -x

    @override
    def NegRat(self, x: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        return -x

    @override
    def PowRat(self, x: _SupportsRat, y: _SupportsInt) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x ** y.__int__()

    @override
    def SubInt(self, x: _SupportsInt, y: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x - y

    @override
    def SubRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x - y
