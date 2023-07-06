import operator

from typing_extensions import override

from ._functools import Operator1, Operator2, Relation2, curry
from .lossabc import ABCLossBuiltins, Number


class LossGodelBuiltins(ABCLossBuiltins):
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
