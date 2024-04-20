import functools
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from fractions import Fraction
from typing import Any, Callable, Generic, Iterable, Tuple, Type, cast

from typing_extensions import TypeAlias, TypeVar, final, override

from ...ast import DType, Tensor
from . import types as vcl

_S = TypeVar("_S")
_T = TypeVar("_T")


@dataclass(frozen=True, init=False)
class Builtins(
    Generic[
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
    ],
    metaclass=ABCMeta,
):
    @abstractmethod
    def Unit(self) -> vcl.Unit: ...

    @abstractmethod
    def Index(self, value: int) -> int: ...

    @abstractmethod
    def BoolTensor(self, value: Tensor[bool]) -> vcl.BoolTensor: ...

    @abstractmethod
    def NatTensor(self, value: Tensor[int]) -> vcl.NatTensor: ...

    @abstractmethod
    def IntTensor(self, value: Tensor[int]) -> vcl.IntTensor: ...

    @abstractmethod
    def RatTensor(self, value: Tensor[Fraction]) -> vcl.RatTensor: ...

    @abstractmethod
    def NilList(self) -> Tuple[_T, ...]: ...

    @abstractmethod
    def ConsList(self, x: _T, xs: Tuple[_T, ...]) -> Tuple[_T, ...]: ...

    @abstractmethod
    def NotBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def AndBoolTensor(self, x: vcl.BoolTensor, y: vcl.BoolTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def OrBoolTensor(self, x: vcl.BoolTensor, y: vcl.BoolTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def NegRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def AddRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def SubRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def MulRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def DivRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def EqRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def NeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def LeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def LtRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def GeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def GtRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def PowRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def MinRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def MaxRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def ReduceAndBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def ReduceOrBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor: ...

    @abstractmethod
    def ReduceSumRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def ReduceRatTensor(
        self,
        f: Callable[[vcl.RatTensor, vcl.RatTensor], vcl.RatTensor],
        x: vcl.RatTensor,
    ) -> vcl.RatTensor: ...

    @abstractmethod
    def EqIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool: ...

    @abstractmethod
    def NeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool: ...

    @abstractmethod
    def LeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool: ...

    @abstractmethod
    def LtIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool: ...

    @abstractmethod
    def GeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool: ...

    @abstractmethod
    def GtIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool: ...

    @abstractmethod
    def LookupRatTensor(self, x: vcl.RatTensor, i: vcl.IndexTensor) -> vcl.Rat: ...

    @abstractmethod
    def StackRatTensor(self, n: int, *xs: vcl.RatTensor) -> vcl.RatTensor: ...

    @abstractmethod
    def ConstRatTensor(self, value: vcl.Rat) -> vcl.RatTensor: ...

    @abstractmethod
    def FoldList(
        self,
        f: Callable[[_T, _S], _T],
        x: _T,
        xs: Tuple[_S, ...],
    ) -> _T: ...

    @abstractmethod
    def MapList(self, f: Callable[[_T], _T], xs: Tuple[_T, ...]) -> Tuple[_T, ...]: ...

    @abstractmethod
    def MapRatTensor(
        self, f: Callable[[vcl.Rat], vcl.Rat], x: vcl.RatTensor
    ) -> vcl.RatTensor: ...

    @abstractmethod
    def ZipWithRatTensor(
        self,
        f: Callable[[vcl.Rat, vcl.Rat], vcl.Rat],
        x: vcl.RatTensor,
        y: vcl.RatTensor,
    ) -> vcl.RatTensor: ...

    @abstractmethod
    def IndicesIndexTensor(self, x: vcl.NatTensor) -> vcl.IndexTensor: ...

    @abstractmethod
    def MinimiseRatTensor(
        self,
        join: Callable[[vcl.RatTensor, vcl.RatTensor], vcl.RatTensor],
        predicate: Callable[..., vcl.RatTensor],
    ) -> vcl.RatTensor: ...

    @abstractmethod
    def MaximiseRatTensor(
        self,
        meet: Callable[[vcl.RatTensor, vcl.RatTensor], vcl.RatTensor],
        predicate: Callable[..., vcl.RatTensor],
    ) -> vcl.RatTensor: ...

    @abstractmethod
    def If(self, cond: vcl.Bool, ifTrue: _T, ifFalse: _T) -> _T: ...


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Builtins[
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
    ],
):
    @override
    def Unit(self) -> Tuple[()]:
        return ()

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def NilList(self) -> Tuple[_T, ...]:
        return ()

    @override
    def ConsList(self, x: _T, xs: Tuple[_T, ...]) -> Tuple[_T, ...]:
        return (x, *xs)

    @override
    def FoldList(
        self,
        f: Callable[[_T, _S], _T],
        x: _T,
        xs: Tuple[_S, ...],
    ) -> _T:
        return cast(
            _T,
            functools.reduce(function=f, sequence=xs, initial=x),  # type: ignore[call-overload]
        )

    @override
    def MapList(self, f: Callable[[_T], _T], xs: Tuple[_T, ...]) -> Tuple[_T, ...]:
        return tuple(map(f, xs))


AnyBuiltins: TypeAlias = ABCBuiltins[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
