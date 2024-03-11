from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Callable, List, SupportsInt, Tuple, Union

from typing_extensions import Generic, Literal, TypeAlias, TypeVar

from .. import ast as vcl

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################


Ix: TypeAlias = int

_Bool = TypeVar("_Bool")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")

_BoolTensor = TypeVar("_BoolTensor")
_NatTensor = TypeVar("_NatTensor")
_IntTensor = TypeVar("_IntTensor")
_RatTensor = TypeVar("_RatTensor")

_AnyTensor = Union[_BoolTensor, _NatTensor, _IntTensor, _RatTensor]

_NumTensor = Union[_NatTensor, _IntTensor, _RatTensor]

Value: TypeAlias = Union[
    Ix,
    _Bool,
    _Nat,
    _Int,
    _Rat,
    _BoolTensor,
    _NatTensor,
    _IntTensor,
    _RatTensor,
    "ValueList",
]

ValueList: TypeAlias = Tuple["Value", ...]

_S = TypeVar("_S", bound=Value)
_T = TypeVar("_T", bound=Value)


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Generic[_Bool, _Nat, _Int, _Rat, _BoolTensor, _NatTensor, _IntTensor, _RatTensor],
    metaclass=ABCMeta,
):
    def Index(self, value: SupportsInt) -> Ix:
        return value.__int__()

    @abstractmethod
    def BoolTensor(self, value: vcl.Tensor[bool]) -> _BoolTensor:
        pass

    @abstractmethod
    def NatTensor(self, value: vcl.Tensor[int]) -> _NatTensor:
        pass

    @abstractmethod
    def IntTensor(self, value: vcl.Tensor[int]) -> _IntTensor:
        pass

    @abstractmethod
    def RatTensor(self, value: vcl.Tensor[vcl.Rat]) -> _RatTensor:
        pass

    def NilList(self) -> ValueList:
        return ()

    def ConsList(self, head: _T, tail: Tuple[_T, ...]) -> Tuple[_T, ...]:
        return (head, *tail)

    @abstractmethod
    def NotTensor(self, x: _BoolTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def AndTensor(self, x: _BoolTensor, y: _BoolTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def OrTensor(self, x: _BoolTensor, y: _BoolTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def NegTensor(self, x: _NumTensor) -> _NumTensor:
        pass

    @abstractmethod
    def AddTensor(self, x: _NumTensor, y: _NumTensor) -> _NumTensor:
        pass

    @abstractmethod
    def SubTensor(self, x: _NumTensor, y: _NumTensor) -> _NumTensor:
        pass

    @abstractmethod
    def MulTensor(self, x: _NumTensor, y: _NumTensor) -> _NumTensor:
        pass

    @abstractmethod
    def DivTensor(self, x: _NumTensor, y: _NumTensor) -> _NumTensor:
        pass

    @abstractmethod
    def EqTensor(self, x: _NumTensor, y: _NumTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def NeTensor(self, x: _NumTensor, y: _NumTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def LeTensor(self, x: _NumTensor, y: _NumTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def LtTensor(self, x: _NumTensor, y: _NumTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def GeTensor(self, x: _NumTensor, y: _NumTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def GtTensor(self, x: _NumTensor, y: _NumTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def EqIndex(self, x: Ix, y: Ix) -> _BoolTensor:
        pass

    @abstractmethod
    def NeIndex(self, x: Ix, y: Ix) -> _BoolTensor:
        pass

    @abstractmethod
    def LeIndex(self, x: Ix, y: Ix) -> _BoolTensor:
        pass

    @abstractmethod
    def LtIndex(self, x: Ix, y: Ix) -> _BoolTensor:
        pass

    @abstractmethod
    def GeIndex(self, x: Ix, y: Ix) -> _BoolTensor:
        pass

    @abstractmethod
    def GtIndex(self, x: Ix, y: Ix) -> _BoolTensor:
        pass

    @abstractmethod
    def ReduceAndTensor(self, x: _BoolTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def ReduceOrTensor(self, x: _BoolTensor) -> _BoolTensor:
        pass

    @abstractmethod
    def ReduceSumTensor(self, x: _NumTensor) -> _NumTensor:
        pass

    @abstractmethod
    def LookupTensor(self, x: _AnyTensor) -> _AnyTensor:
        pass

    @abstractmethod
    def PowRatTensor(self, x: _RatTensor, y: _IntTensor) -> _RatTensor:
        pass

    @abstractmethod
    def MinRatTensor(self, x: _RatTensor) -> _RatTensor:
        pass

    @abstractmethod
    def MaxRatTensor(self, x: _RatTensor) -> _RatTensor:
        pass

    @abstractmethod
    def StackTensor(self, xs: Tuple[_AnyTensor, ...]) -> _AnyTensor:
        pass

    @abstractmethod
    def FoldList(
        self,
        func: Callable[[_S, _T], _T],
        init: _T,
        vals: Tuple[_S, ...],
    ) -> _T:
        pass

    @abstractmethod
    def FoldTensor(self):
        pass

    @abstractmethod
    def MapList(self):
        pass

    @abstractmethod
    def MapTensor(self):
        pass

    @abstractmethod
    def ZipWithTensor(self):
        pass

    @abstractmethod
    def Indices(self):
        pass

    @abstractmethod
    def Optimise(self, minimiseOrMaximise: Literal["Minimise", "Maximise"]):
        pass

    @abstractmethod
    def If(self):
        pass

    @abstractmethod
    def Forall(self):
        pass

    @abstractmethod
    def Exists(self):
        pass
