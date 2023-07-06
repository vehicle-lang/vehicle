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


class LossDL2Builtins(ABCLossBuiltins):
    samplers: Dict[str, Callable[[Dict[str, Any]], Any]]

    @override
    def And(self) -> Operator2[float]:
        return curry(operator.add)

    @override
    def Bool(self, value: bool) -> float:
        return 0.0 if value else 1.0

    @override
    def Eq(self) -> Relation2[Number, float]:
        return lambda x: lambda y: -abs(x - y)

    @override
    def Exists(self) -> Function1[Function1[_T, float], float]:
        return NotImplemented

    @override
    def Forall(self) -> Function1[Function1[_T, float], float]:
        return NotImplemented

    @override
    def GeInt(self) -> Relation2[int, float]:
        return curry(operator.ge)

    @override
    def GeRat(self) -> Relation2[float, float]:
        return curry(operator.ge)

    @override
    def GtIndex(self) -> Relation2[int, float]:
        return curry(operator.gt)

    @override
    def GtNat(self) -> Relation2[int, float]:
        return curry(operator.gt)

    @override
    def Implies(self) -> Operator2[float]:
        return lambda x: lambda y: (not x) or y

    @override
    def LeInt(self) -> Relation2[int, float]:
        return curry(operator.le)

    @override
    def LeRat(self) -> Relation2[float, float]:
        return curry(operator.le)

    @override
    def LtInt(self) -> Relation2[int, float]:
        return curry(operator.lt)

    @override
    def LtRat(self) -> Relation2[float, float]:
        return curry(operator.lt)

    @override
    def Ne(self) -> Relation2[Number, float]:
        return curry(operator.ne)

    @override
    def Not(self) -> Operator1[float]:
        return operator.not_

    @override
    def Or(self) -> Operator2[float]:
        return curry(operator.or_)
