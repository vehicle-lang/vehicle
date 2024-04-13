from fractions import Fraction
from typing import Callable, Type

import numpy as np
from typing_extensions import Literal, override

from ...ast import Tensor
from ..abc import ABCBuiltins, Value
from . import types as vcl


class NumpyBuiltins(
    ABCBuiltins[
        vcl.Index,
        vcl.Bool,
        vcl.Nat,
        vcl.Int,
        vcl.Rat,
        vcl.IndexTensor,
        vcl.BoolTensor,
        vcl.NatTensor,
        vcl.IntTensor,
        vcl.RatTensor,
    ]
):
    @override
    def IndexType(self) -> Type[vcl.Index]:
        return vcl.Index

    @override
    def BoolTensorType(self) -> Type[vcl.BoolTensor]:
        return vcl.BoolTensor

    @override
    def IndexTensorType(self) -> Type[vcl.IndexTensor]:
        return vcl.IndexTensor

    @override
    def NatTensorType(self) -> Type[vcl.NatTensor]:
        return vcl.NatTensor

    @override
    def IntTensorType(self) -> Type[vcl.IntTensor]:
        return vcl.IntTensor

    @override
    def RatTensorType(self) -> Type[vcl.RatTensor]:
        return vcl.RatTensor

    @override
    def BoolTensor(self, value: Tensor[bool]) -> vcl.BoolTensor:
        return np.reshape(np.array(value.value, dtype=bool), value.shape)

    @override
    def NatTensor(self, value: Tensor[int]) -> vcl.NatTensor:
        return np.reshape(np.array(value.value, dtype=np.uint), value.shape)

    @override
    def IntTensor(self, value: Tensor[int]) -> vcl.IntTensor:
        return np.reshape(np.array(value.value, dtype=np.int_), value.shape)

    @override
    def RatTensor(self, value: Tensor[Fraction]) -> vcl.RatTensor:
        return np.reshape(np.array(value.value, dtype=np.float_), value.shape)

    @override
    def NotBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor:
        return np.logical_not(x)

    @override
    def AndBoolTensor(self, x: vcl.BoolTensor, y: vcl.BoolTensor) -> vcl.BoolTensor:
        return np.logical_and(x, y)

    @override
    def OrBoolTensor(self, x: vcl.BoolTensor, y: vcl.BoolTensor) -> vcl.BoolTensor:
        return np.logical_or(x, y)

    @override
    def NegRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor:
        return np.negative(x)

    @override
    def AddRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return np.add(x, y)

    @override
    def SubRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return np.subtract(x, y)

    @override
    def MulRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return np.multiply(x, y)

    @override
    def DivRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return np.divide(x, y)

    @override
    def EqRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return np.equal(x, y)

    @override
    def NeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return np.not_equal(x, y)

    @override
    def LeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return np.less_equal(x, y)

    @override
    def LtRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return np.less(x, y)

    @override
    def GeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return np.greater_equal(x, y)

    @override
    def GtRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return np.greater(x, y)

    @override
    def PowRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return np.power(x, y)

    @override
    def MinRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor:
        return np.min(x)

    @override
    def MaxRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor:
        return np.max(x)

    @override
    def ReduceAndBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor:
        return np.array((np.all(x),), dtype=bool)

    @override
    def ReduceOrBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor:
        return np.array((np.all(x),), np.any(x))

    @override
    def ReduceSumRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor:
        return np.sum(x)

    @override
    def ReduceRatTensor(
        self,
        f: Callable[[vcl.RatTensor, vcl.RatTensor], vcl.RatTensor],
        x: vcl.RatTensor,
    ) -> vcl.RatTensor:
        return NotImplemented

    @override
    def EqIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x == y

    @override
    def NeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x != y

    @override
    def LeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x <= y

    @override
    def LtIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x < y

    @override
    def GeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x >= y

    @override
    def GtIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x > y

    @override
    def LookupRatTensor(self, x: vcl.RatTensor, i: vcl.IndexTensor) -> vcl.Rat:
        return x[i]

    @override
    def StackRatTensor(self, n: int, *xs: vcl.RatTensor) -> vcl.RatTensor:
        return np.stack(xs, axis=0)

    @override
    def ConstRatTensor(self, value: vcl.Rat) -> vcl.RatTensor:
        return NotImplemented

    @override
    def MapRatTensor(
        self, f: Callable[[vcl.Rat], vcl.Rat], x: vcl.RatTensor
    ) -> vcl.RatTensor:
        return NotImplemented

    @override
    def ZipWithRatTensor(
        self,
        f: Callable[[vcl.Rat, vcl.Rat], vcl.Rat],
        x: vcl.RatTensor,
        y: vcl.RatTensor,
    ) -> vcl.RatTensor:
        return NotImplemented

    @override
    def IndicesIndexTensor(self, x: vcl.NatTensor) -> vcl.IndexTensor:
        return x

    @override
    def OptimiseRatTensor(
        self,
        minimiseOrMaximise: Literal["Minimise", "Maximise"],
        meetOrJoin: Callable[[vcl.RatTensor, vcl.RatTensor], vcl.RatTensor],
        loss: Callable[[Value], vcl.RatTensor],
    ) -> vcl.RatTensor:
        return NotImplemented

    @override
    def If(self, cond: vcl.Bool, ifTrue: Value, ifFalse: Value) -> Value:
        return NotImplemented
