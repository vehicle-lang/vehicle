import functools
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Any, Callable, Generic, Tuple, cast

from typing_extensions import TypeAlias, TypeVar, override

from ...ast import Tensor
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
        vcl.Tensor,
    ],
    metaclass=ABCMeta,
):
    @abstractmethod
    def Unit(self) -> vcl.Unit: ...

    @abstractmethod
    def Index(self, value: int) -> int: ...

    @abstractmethod
    def BoolTensor(self, value: Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def NatTensor(self, value: Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def IntTensor(self, value: Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def RatTensor(self, value: Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def NilList(self) -> Tuple[object, ...]: ...

    @abstractmethod
    def ConsList(self, x: _T, xs: Tuple[_T, ...]) -> Tuple[_T, ...]: ...

    @abstractmethod
    def NotBoolTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def AndBoolTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def OrBoolTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def NegRatTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def AddRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def SubRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def MulRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def DivRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def EqRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def NeRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def LeRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def LtRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def GeRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def GtRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def PowRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def MinRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def MaxRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceAndBoolTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceOrBoolTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceSumRatTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceRatTensor(
        self,
        f: Callable[[vcl.Tensor, vcl.Tensor], vcl.Tensor],
        x: vcl.Tensor,
    ) -> vcl.Tensor: ...

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
    def LookupRatTensor(self, x: vcl.Tensor, i: vcl.Tensor) -> vcl.Rat: ...

    @abstractmethod
    def StackRatTensor(self, n: int, *xs: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ConstRatTensor(self, value: vcl.Rat) -> vcl.Tensor: ...

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
        self, f: Callable[[vcl.Rat], vcl.Rat], x: vcl.Tensor
    ) -> vcl.Tensor: ...

    @abstractmethod
    def ZipWithRatTensor(
        self,
        f: Callable[[vcl.Rat, vcl.Rat], vcl.Rat],
        x: vcl.Tensor,
        y: vcl.Tensor,
    ) -> vcl.Tensor: ...

    @abstractmethod
    def IndicesIndexTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def MinimiseRatTensor(
        self,
        join: Callable[[vcl.Tensor, vcl.Tensor], vcl.Tensor],
        predicate: Callable[..., vcl.Tensor],
    ) -> vcl.Tensor: ...

    @abstractmethod
    def MaximiseRatTensor(
        self,
        meet: Callable[[vcl.Tensor, vcl.Tensor], vcl.Tensor],
        predicate: Callable[..., vcl.Tensor],
    ) -> vcl.Tensor: ...

    @abstractmethod
    def If(self, cond: vcl.Bool, ifTrue: _T, ifFalse: _T) -> _T: ...

    @abstractmethod
    def DimensionLookup(self, xs: vcl.Tensor, i: vcl.Index) -> vcl.Nat: ...

    @abstractmethod
    def DimensionCons(
        self, head: vcl.Nat, tail: Tuple[vcl.Nat, ...]
    ) -> Tuple[vcl.Nat, ...]: ...

    @abstractmethod
    def DimensionNil(self) -> Tuple[vcl.Nat, ...]: ...

    @abstractmethod
    def ConstTensor(self, value: vcl.Rat, shape: Tuple[vcl.Nat, ...]) -> vcl.Tensor: ...

    @abstractmethod
    def DenseTensor(
        self, values: Tuple[vcl.Rat, ...], shape: Tuple[vcl.Nat, ...]
    ) -> vcl.Tensor: ...


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Builtins[
        vcl.Index,
        vcl.Bool,
        vcl.Nat,
        vcl.Int,
        vcl.Rat,
        vcl.Tensor,
    ],
):
    @override
    def Unit(self) -> Tuple[()]:
        return ()

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def NilList(self) -> Tuple[object, ...]:
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

    @override
    def DimensionLookup(self, xs: vcl.Tensor, i: vcl.Index) -> vcl.Nat:
        raise NotImplementedError(
            "DimensionLookup requires concrete tensor implementation"
        )

    @override
    def DimensionCons(
        self, head: vcl.Nat, tail: Tuple[vcl.Nat, ...]
    ) -> Tuple[vcl.Nat, ...]:
        return (head, *tail)

    @override
    def DimensionNil(self) -> Tuple[vcl.Nat, ...]:
        return ()

    @override
    def ConstTensor(self, value: vcl.Rat, shape: Tuple[vcl.Nat, ...]) -> vcl.Tensor:
        raise NotImplementedError("ConstTensor requires concrete tensor implementation")

    @override
    def DenseTensor(
        self, values: Tuple[vcl.Rat, ...], shape: Tuple[vcl.Nat, ...]
    ) -> vcl.Tensor:
        raise NotImplementedError("DenseTensor requires concrete tensor implementation")


AnyBuiltins: TypeAlias = ABCBuiltins[Any, Any, Any, Any, Any, Any]
