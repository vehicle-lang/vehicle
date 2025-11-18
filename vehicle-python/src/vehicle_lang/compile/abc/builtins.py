from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Any, Generic, Sequence

from typing_extensions import TypeAlias, TypeVar, override

from ...ast import Tensor
from . import types as vcl

_S = TypeVar("_S")
_T = TypeVar("_T")


@dataclass(frozen=True, init=False)
class Builtins(
    Generic[
        vcl.Index,
        vcl.Rat,
        vcl.Tensor,
    ],
    metaclass=ABCMeta,
):
    @abstractmethod
    def Index(self, value: int) -> int: ...

    @abstractmethod
    def RatTensor(self, value: Tensor) -> vcl.Tensor: ...

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
    def MinRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def MaxRatTensor(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceAddRatTensor(self, e: vcl.Rat, xs: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceMulRatTensor(self, e: vcl.Rat, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceMinRatTensor(self, e: vcl.Rat, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceMaxRatTensor(self, e: vcl.Rat, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def DimensionLookup(self, xs: vcl.Tensor, i: vcl.Index) -> vcl.Index: ...

    @abstractmethod
    def DimensionCons(
        self, head: vcl.Index, tail: Sequence[vcl.Index]
    ) -> Sequence[vcl.Index]: ...

    @abstractmethod
    def DimensionNil(self) -> Sequence[vcl.Index]: ...

    @abstractmethod
    def StackTensor(self, tensors: Sequence[vcl.Tensor]) -> vcl.Tensor: ...

    @abstractmethod
    def ConstTensor(self, value: vcl.Rat, shape: Sequence[vcl.Index]) -> vcl.Tensor: ...

    @abstractmethod
    def DenseTensor(
        self, values: Sequence[vcl.Rat], shape: Sequence[vcl.Index]
    ) -> vcl.Tensor: ...


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Builtins[
        vcl.Index,
        vcl.Rat,
        vcl.Tensor,
    ],
):

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def DimensionLookup(self, xs: vcl.Tensor, i: vcl.Index) -> vcl.Index:
        raise NotImplementedError(
            "DimensionLookup requires concrete tensor implementation"
        )

    @override
    def DimensionCons(
        self, head: vcl.Index, tail: Sequence[vcl.Index]
    ) -> Sequence[vcl.Index]:
        # Preserve the sequence type of the tail by reconstructing with the same type
        tail_type = type(tail)
        try:
            # Try to construct a new sequence of the same type
            # This works for most sequence types that accept iterables
            return tail_type([head, *tail])  # type: ignore
        except (TypeError, ValueError):
            # Fallback: for immutable sequences like tuple, range, etc.
            # that don't accept list initialization, try unpacking
            try:
                return tail_type((head, *tail))  # type: ignore
            except (TypeError, ValueError):
                # Last resort: return as tuple (most compatible immutable sequence)
                return (head, *tail)

    @override
    def DimensionNil(self) -> Sequence[vcl.Index]:
        # Use tuple as the default empty sequence type
        # Concrete implementations can override this if needed
        return ()

    @override
    def ConstTensor(self, value: vcl.Rat, shape: Sequence[vcl.Index]) -> vcl.Tensor:
        raise NotImplementedError("ConstTensor requires concrete tensor implementation")

    @override
    def DenseTensor(
        self, values: Sequence[vcl.Rat], shape: Sequence[vcl.Index]
    ) -> vcl.Tensor:
        raise NotImplementedError("DenseTensor requires concrete tensor implementation")


AnyBuiltins: TypeAlias = ABCBuiltins[Any, Any, Any]
