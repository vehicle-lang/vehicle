import operator
from typing import Any, Callable, Dict

from typing_extensions import TypeVar, override

from ._functools import (
    Function1,
    Function2,
    Function3,
    Operator1,
    Operator2,
    Relation2,
    curry,
)
from .lossabc import ABCLossBuiltins, Number

_T = TypeVar("_T")


class LossGodelBuiltins(ABCLossBuiltins):
    samplers: Dict[str, Callable[[Dict[str, Any]], Any]]

    @override
    def And(self) -> Operator2[float]:
        return curry(min)

    @override
    def Bool(self, value: bool) -> float:
        return float(value)

    @override
    def Eq(self) -> Relation2[Number, float]:
        return lambda x: lambda y: 1 - abs(x - y / x + y)

    @override
    def Exists(self) -> Function1[Function1[_T, float], float]:
        return NotImplemented

    @override
    def Forall(self) -> Function1[Function1[_T, float], float]:
        return NotImplemented

    @override
    def LtInt(self) -> Relation2[int, float]:
        return lambda x: lambda y: 1 - max(x - y / x + y, 0)

    @override
    def LtRat(self) -> Relation2[float, float]:
        return lambda x: lambda y: 1 - max(x - y / x + y, 0)

    @override
    def Not(self) -> Operator1[float]:
        return operator.neg

    @override
    def Or(self) -> Operator2[float]:
        return curry(max)
