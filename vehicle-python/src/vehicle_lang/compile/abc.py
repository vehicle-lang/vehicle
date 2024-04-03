import functools
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from fractions import Fraction
from typing import Any, Callable, Dict, Generic, Tuple, Type, Union

from typing_extensions import Literal, TypeAlias, TypeVar, override

from .. import ast as vcl
from ..typing import Optimiser

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

_Bool = TypeVar("_Bool")
_Index = TypeVar("_Index")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")
_BoolTensor = TypeVar("_BoolTensor")
_IndexTensor = TypeVar("_IndexTensor")
_NatTensor = TypeVar("_NatTensor")
_IntTensor = TypeVar("_IntTensor")
_RatTensor = TypeVar("_RatTensor")

Unit: TypeAlias = Tuple[()]

Value: TypeAlias = Union[
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


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Generic[
        _Index,
        _Bool,
        _Nat,
        _Int,
        _Rat,
        _IndexTensor,
        _BoolTensor,
        _NatTensor,
        _IntTensor,
        _RatTensor,
    ],
    metaclass=ABCMeta,
):
    optimisers: Dict[str, Optimiser[Any, _Rat]] = field(default_factory=dict)

    @abstractmethod
    def IndexType(self) -> Type[_Index]: ...

    @abstractmethod
    def BoolTensorType(self) -> Type[_BoolTensor]: ...

    @abstractmethod
    def IndexTensorType(self) -> Type[_IndexTensor]: ...

    @abstractmethod
    def NatTensorType(self) -> Type[_NatTensor]: ...

    @abstractmethod
    def IntTensorType(self) -> Type[_IntTensor]: ...

    @abstractmethod
    def RatTensorType(self) -> Type[_RatTensor]: ...

    def ListType(self) -> Type[ValueList]:
        return ValueList

    def Unit(self) -> Tuple[()]:
        return ()

    def Index(self, value: int) -> int:
        return value

    @abstractmethod
    def BoolTensor(self, value: vcl.Tensor[bool]) -> _BoolTensor: ...

    @abstractmethod
    def NatTensor(self, value: vcl.Tensor[int]) -> _NatTensor: ...

    @abstractmethod
    def IntTensor(self, value: vcl.Tensor[int]) -> _IntTensor: ...

    @abstractmethod
    def RatTensor(self, value: vcl.Tensor[Fraction]) -> _RatTensor: ...

    def NilList(self) -> ValueList:
        return ()

    def ConsList(self, x: Value, xs: ValueList) -> ValueList:
        return (x, *xs)

    @abstractmethod
    def NotBoolTensor(self, x: _BoolTensor) -> _BoolTensor: ...

    @abstractmethod
    def AndBoolTensor(self, x: _BoolTensor, y: _BoolTensor) -> _BoolTensor: ...

    @abstractmethod
    def OrBoolTensor(self, x: _BoolTensor, y: _BoolTensor) -> _BoolTensor: ...

    @abstractmethod
    def NegRatTensor(self, x: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def AddRatTensor(self, x: _RatTensor, y: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def SubRatTensor(self, x: _RatTensor, y: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def MulRatTensor(self, x: _RatTensor, y: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def DivRatTensor(self, x: _RatTensor, y: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def EqRatTensor(self, x: _RatTensor, y: _RatTensor) -> _BoolTensor: ...

    @abstractmethod
    def NeRatTensor(self, x: _RatTensor, y: _RatTensor) -> _BoolTensor: ...

    @abstractmethod
    def LeRatTensor(self, x: _RatTensor, y: _RatTensor) -> _BoolTensor: ...

    @abstractmethod
    def LtRatTensor(self, x: _RatTensor, y: _RatTensor) -> _BoolTensor: ...

    @abstractmethod
    def GeRatTensor(self, x: _RatTensor, y: _RatTensor) -> _BoolTensor: ...

    @abstractmethod
    def GtRatTensor(self, x: _RatTensor, y: _RatTensor) -> _BoolTensor: ...

    @abstractmethod
    def PowRatTensor(self, x: _RatTensor, y: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def MinRatTensor(self, x: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def MaxRatTensor(self, x: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def ReduceAndBoolTensor(self, x: _BoolTensor) -> _BoolTensor: ...

    @abstractmethod
    def ReduceOrBoolTensor(self, x: _BoolTensor) -> _BoolTensor: ...

    @abstractmethod
    def ReduceSumRatTensor(self, x: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def ReduceRatTensor(
        self, f: Callable[[_RatTensor, _RatTensor], _RatTensor], x: _RatTensor
    ) -> _RatTensor: ...

    @abstractmethod
    def EqIndex(self, x: _Index, y: _Index) -> _Bool: ...

    @abstractmethod
    def NeIndex(self, x: _Index, y: _Index) -> _Bool: ...

    @abstractmethod
    def LeIndex(self, x: _Index, y: _Index) -> _Bool: ...

    @abstractmethod
    def LtIndex(self, x: _Index, y: _Index) -> _Bool: ...

    @abstractmethod
    def GeIndex(self, x: _Index, y: _Index) -> _Bool: ...

    @abstractmethod
    def GtIndex(self, x: _Index, y: _Index) -> _Bool: ...

    @abstractmethod
    def LookupRatTensor(self, x: _RatTensor, i: _IndexTensor) -> _Rat: ...

    @abstractmethod
    def StackRatTensor(self, n: int, *xs: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def ConstRatTensor(self, value: _Rat) -> _RatTensor: ...

    def FoldList(
        self, f: Callable[[Value, Value], Value], x: Value, xs: ValueList
    ) -> Value:
        return functools.reduce(f, xs, initial=x)

    def MapList(self, f: Callable[[Value], Value], xs: ValueList) -> ValueList:
        return tuple(map(f, xs))

    @abstractmethod
    def MapRatTensor(self, f: Callable[[_Rat], _Rat], x: _RatTensor) -> _RatTensor: ...

    @abstractmethod
    def ZipWithRatTensor(
        self, f: Callable[[_Rat, _Rat], _Rat], x: _RatTensor, y: _RatTensor
    ) -> _RatTensor: ...

    @abstractmethod
    def IndicesIndexTensor(self, x: _NatTensor) -> _IndexTensor: ...

    @abstractmethod
    def OptimiseRatTensor(
        self,
        minimiseOrMaximise: Literal["Minimise", "Maximise"],
        meetOrJoin: Callable[[_RatTensor, _RatTensor], _RatTensor],
        loss: Callable[[Value], _RatTensor],
    ) -> _RatTensor: ...

    @abstractmethod
    def If(self, cond: _Bool, ifTrue: Value, ifFalse: Value) -> Value: ...


AnyBuiltins: TypeAlias = ABCBuiltins[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]

################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


_Program = TypeVar("_Program")
_Declaration = TypeVar("_Declaration")
_Expression = TypeVar("_Expression")


class Translation(Generic[_Program, _Declaration, _Expression], metaclass=ABCMeta):
    @abstractmethod
    def translate_program(self, program: vcl.Program) -> _Program: ...

    @abstractmethod
    def translate_declaration(self, declaration: vcl.Declaration) -> _Declaration: ...

    @abstractmethod
    def translate_expression(self, expression: vcl.Expression) -> _Expression: ...


class ABCTranslation(Translation[_Program, _Declaration, _Expression]):
    @override
    def translate_program(self, program: vcl.Program) -> _Program:
        if isinstance(program, vcl.Main):
            return self.translate_Main(program)
        raise NotImplementedError(type(program).__name__)

    @abstractmethod
    def translate_Main(self, program: vcl.Main) -> _Program: ...

    @override
    def translate_declaration(self, declaration: vcl.Declaration) -> _Declaration:
        if isinstance(declaration, vcl.DefFunction):
            return self.translate_DefFunction(declaration)
        if isinstance(declaration, vcl.DefPostulate):
            return self.translate_DefPostulate(declaration)
        raise NotImplementedError(type(declaration).__name__)

    @abstractmethod
    def translate_DefFunction(self, declaration: vcl.DefFunction) -> _Declaration: ...

    @abstractmethod
    def translate_DefPostulate(self, declaration: vcl.DefPostulate) -> _Declaration: ...

    @override
    def translate_expression(self, expression: vcl.Expression) -> _Expression:
        if isinstance(expression, vcl.App):
            return self.translate_App(expression)
        if isinstance(expression, vcl.BoundVar):
            return self.translate_BoundVar(expression)
        if isinstance(expression, vcl.Builtin):
            return self.translate_Builtin(expression)
        if isinstance(expression, vcl.FreeVar):
            return self.translate_FreeVar(expression)
        if isinstance(expression, vcl.Lam):
            return self.translate_Lam(expression)
        if isinstance(expression, vcl.Let):
            return self.translate_Let(expression)
        if isinstance(expression, vcl.PartialApp):
            return self.translate_PartialApp(expression)
        if isinstance(expression, vcl.Pi):
            return self.translate_Pi(expression)
        if isinstance(expression, vcl.Universe):
            return self.translate_Universe(expression)
        raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: vcl.App) -> _Expression: ...

    @abstractmethod
    def translate_BoundVar(self, expression: vcl.BoundVar) -> _Expression: ...

    @abstractmethod
    def translate_Builtin(self, expression: vcl.Builtin) -> _Expression: ...

    @abstractmethod
    def translate_FreeVar(self, expression: vcl.FreeVar) -> _Expression: ...

    @abstractmethod
    def translate_Lam(self, expression: vcl.Lam) -> _Expression: ...

    def translate_Let(self, expression: vcl.Let) -> _Expression:
        return self.translate_expression(
            vcl.App(
                provenance=expression.provenance,
                function=vcl.Lam(
                    provenance=expression.provenance,
                    binders=(expression.binder,),
                    body=expression.body,
                ),
                arguments=[expression.bound],
            )
        )

    @abstractmethod
    def translate_PartialApp(self, expression: vcl.PartialApp) -> _Expression: ...

    @abstractmethod
    def translate_Pi(self, expression: vcl.Pi) -> _Expression: ...

    @abstractmethod
    def translate_Universe(self, expression: vcl.Universe) -> _Expression: ...
