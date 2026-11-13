from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Any, Callable, Generic, List, Sequence

from typing_extensions import TypeAlias, TypeVar, override

from ..._ast._nodes import ExtendedFraction, Tensor
from . import _types as vcl


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Generic[
        vcl.Index,
        vcl.Rat,
        vcl.Tensor,
        vcl.Vector,
    ],
    metaclass=ABCMeta,
):
    def Index(self, value: int) -> int:
        return value

    @abstractmethod
    def BoolTensor(self, x: Tensor[bool]) -> vcl.Tensor: ...

    @abstractmethod
    def BoolNot(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def BoolAnd(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def BoolOr(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def BoolImplies(self, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def BoolCompareIndex(self, op: str, x: vcl.Index, y: vcl.Index) -> vcl.Tensor: ...

    @abstractmethod
    def BoolCompareNat(self, op: str, x: vcl.Index, y: vcl.Index) -> vcl.Tensor: ...

    @abstractmethod
    def BoolCompareRatTensor(
        self,
        op: str,
        pointwise_dims: Sequence[vcl.Index],
        reduce_dims: Sequence[vcl.Index],
        x: vcl.Tensor,
        y: vcl.Tensor,
    ) -> vcl.Tensor: ...

    @abstractmethod
    def BoolReduceAnd(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def BoolReduceOr(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def BoolIf(self, c: vcl.Tensor, x: vcl.Tensor, y: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def RatTensor(self, x: Tensor[ExtendedFraction]) -> vcl.Tensor: ...

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
    def PowRatTensor(self, x: vcl.Tensor, y: vcl.Rat) -> vcl.Tensor: ...

    @abstractmethod
    def LogRatTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ExpRatTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceAddRatTensor(self, xs: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceMulRatTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceMinRatTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def ReduceMaxRatTensor(self, x: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def WhereTensor(
        self, input: vcl.Tensor, condition: vcl.Tensor, false_value: vcl.Tensor
    ) -> vcl.Tensor: ...

    @abstractmethod
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

    @abstractmethod
    def DimensionNil(self) -> Sequence[vcl.Index]:
        # Use tuple as the default empty sequence type
        # Concrete implementations can override this if needed
        return ()

    @abstractmethod
    def Transpose(self, xs: vcl.Tensor) -> vcl.Tensor: ...

    @abstractmethod
    def StackTensor(self, tensors: Sequence[vcl.Tensor]) -> vcl.Tensor: ...

    @abstractmethod
    def ConstTensor(self, value: vcl.Rat, shape: Sequence[vcl.Index]) -> vcl.Tensor: ...

    @abstractmethod
    def AtTensor(self, xs: vcl.Tensor, i: vcl.Index) -> vcl.Tensor: ...

    @abstractmethod
    def ForeachTensor(
        self, size: int, function: Callable[[int], vcl.Tensor]
    ) -> vcl.Tensor: ...

    @abstractmethod
    def VectorLiteral(self, xs: Sequence[Any]) -> vcl.Vector: ...

    @abstractmethod
    def AtVector(self, xs: vcl.Vector, i: vcl.Index) -> Any: ...

    @abstractmethod
    def ForeachVector(
        self, size: int, function: Callable[[int], Any]
    ) -> vcl.Vector: ...

    @abstractmethod
    def DenseTensor(
        self, values: Sequence[vcl.Rat], shape: Sequence[vcl.Index]
    ) -> vcl.Tensor: ...


AnyBuiltins: TypeAlias = ABCBuiltins[Any, Any, Any, Any]
