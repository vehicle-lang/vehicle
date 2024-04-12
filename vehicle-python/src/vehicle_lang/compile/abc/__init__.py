import functools
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from fractions import Fraction
from typing import Any, Callable, Dict, Generic, Tuple, Type

from typing_extensions import Literal, TypeAlias, override

from ... import ast as vcl_ast
from ...typing import Optimiser
from . import typing as vcl_var


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Generic[
        vcl_var.Index,
        vcl_var.Bool,
        vcl_var.Nat,
        vcl_var.Int,
        vcl_var.Rat,
        vcl_var.IndexTensor,
        vcl_var.BoolTensor,
        vcl_var.NatTensor,
        vcl_var.IntTensor,
        vcl_var.RatTensor,
    ],
    metaclass=ABCMeta,
):
    optimisers: Dict[str, Optimiser[Any, vcl_var.Rat]] = field(default_factory=dict)

    @abstractmethod
    def IndexType(self) -> Type[vcl_var.Index]: ...

    @abstractmethod
    def BoolTensorType(self) -> Type[vcl_var.BoolTensor]: ...

    @abstractmethod
    def IndexTensorType(self) -> Type[vcl_var.IndexTensor]: ...

    @abstractmethod
    def NatTensorType(self) -> Type[vcl_var.NatTensor]: ...

    @abstractmethod
    def IntTensorType(self) -> Type[vcl_var.IntTensor]: ...

    @abstractmethod
    def RatTensorType(self) -> Type[vcl_var.RatTensor]: ...

    def ListType(self) -> Type[vcl_var.ValueList]:
        return vcl_var.ValueList

    def Unit(self) -> Tuple[()]:
        return ()

    def Index(self, value: int) -> int:
        return value

    @abstractmethod
    def BoolTensor(self, value: vcl_ast.Tensor[bool]) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def NatTensor(self, value: vcl_ast.Tensor[int]) -> vcl_var.NatTensor: ...

    @abstractmethod
    def IntTensor(self, value: vcl_ast.Tensor[int]) -> vcl_var.IntTensor: ...

    @abstractmethod
    def RatTensor(self, value: vcl_ast.Tensor[Fraction]) -> vcl_var.RatTensor: ...

    def NilList(self) -> vcl_var.ValueList:
        return ()

    def ConsList(self, x: vcl_var.Value, xs: vcl_var.ValueList) -> vcl_var.ValueList:
        return (x, *xs)

    @abstractmethod
    def NotBoolTensor(self, x: vcl_var.BoolTensor) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def AndBoolTensor(
        self, x: vcl_var.BoolTensor, y: vcl_var.BoolTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def OrBoolTensor(
        self, x: vcl_var.BoolTensor, y: vcl_var.BoolTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def NegRatTensor(self, x: vcl_var.RatTensor) -> vcl_var.RatTensor: ...

    @abstractmethod
    def AddRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def SubRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def MulRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def DivRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def EqRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def NeRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def LeRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def LtRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def GeRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def GtRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def PowRatTensor(
        self, x: vcl_var.RatTensor, y: vcl_var.RatTensor
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def MinRatTensor(self, x: vcl_var.RatTensor) -> vcl_var.RatTensor: ...

    @abstractmethod
    def MaxRatTensor(self, x: vcl_var.RatTensor) -> vcl_var.RatTensor: ...

    @abstractmethod
    def ReduceAndBoolTensor(self, x: vcl_var.BoolTensor) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def ReduceOrBoolTensor(self, x: vcl_var.BoolTensor) -> vcl_var.BoolTensor: ...

    @abstractmethod
    def ReduceSumRatTensor(self, x: vcl_var.RatTensor) -> vcl_var.RatTensor: ...

    @abstractmethod
    def ReduceRatTensor(
        self,
        f: Callable[[vcl_var.RatTensor, vcl_var.RatTensor], vcl_var.RatTensor],
        x: vcl_var.RatTensor,
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def EqIndex(self, x: vcl_var.Index, y: vcl_var.Index) -> vcl_var.Bool: ...

    @abstractmethod
    def NeIndex(self, x: vcl_var.Index, y: vcl_var.Index) -> vcl_var.Bool: ...

    @abstractmethod
    def LeIndex(self, x: vcl_var.Index, y: vcl_var.Index) -> vcl_var.Bool: ...

    @abstractmethod
    def LtIndex(self, x: vcl_var.Index, y: vcl_var.Index) -> vcl_var.Bool: ...

    @abstractmethod
    def GeIndex(self, x: vcl_var.Index, y: vcl_var.Index) -> vcl_var.Bool: ...

    @abstractmethod
    def GtIndex(self, x: vcl_var.Index, y: vcl_var.Index) -> vcl_var.Bool: ...

    @abstractmethod
    def LookupRatTensor(
        self, x: vcl_var.RatTensor, i: vcl_var.IndexTensor
    ) -> vcl_var.Rat: ...

    @abstractmethod
    def StackRatTensor(self, n: int, *xs: vcl_var.RatTensor) -> vcl_var.RatTensor: ...

    @abstractmethod
    def ConstRatTensor(self, value: vcl_var.Rat) -> vcl_var.RatTensor: ...

    def FoldList(
        self,
        f: Callable[[vcl_var.Value, vcl_var.Value], vcl_var.Value],
        x: vcl_var.Value,
        xs: vcl_var.ValueList,
    ) -> vcl_var.Value:
        return functools.reduce(f, xs, initial=x)

    def MapList(
        self, f: Callable[[vcl_var.Value], vcl_var.Value], xs: vcl_var.ValueList
    ) -> vcl_var.ValueList:
        return tuple(map(f, xs))

    @abstractmethod
    def MapRatTensor(
        self, f: Callable[[vcl_var.Rat], vcl_var.Rat], x: vcl_var.RatTensor
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def ZipWithRatTensor(
        self,
        f: Callable[[vcl_var.Rat, vcl_var.Rat], vcl_var.Rat],
        x: vcl_var.RatTensor,
        y: vcl_var.RatTensor,
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def IndicesIndexTensor(self, x: vcl_var.NatTensor) -> vcl_var.IndexTensor: ...

    @abstractmethod
    def OptimiseRatTensor(
        self,
        minimiseOrMaximise: Literal["Minimise", "Maximise"],
        meetOrJoin: Callable[[vcl_var.RatTensor, vcl_var.RatTensor], vcl_var.RatTensor],
        loss: Callable[[vcl_var.Value], vcl_var.RatTensor],
    ) -> vcl_var.RatTensor: ...

    @abstractmethod
    def If(
        self, cond: vcl_var.Bool, ifTrue: vcl_var.Value, ifFalse: vcl_var.Value
    ) -> vcl_var.Value: ...


AnyBuiltins: TypeAlias = ABCBuiltins[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]

################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


class Translation(
    Generic[vcl_var.Program, vcl_var.Declaration, vcl_var.Expression], metaclass=ABCMeta
):
    @abstractmethod
    def translate_program(self, program: vcl_ast.Program) -> vcl_var.Program: ...

    @abstractmethod
    def translate_declaration(
        self, declaration: vcl_ast.Declaration
    ) -> vcl_var.Declaration: ...

    @abstractmethod
    def translate_expression(
        self, expression: vcl_ast.Expression
    ) -> vcl_var.Expression: ...


class ABCTranslation(
    Translation[vcl_var.Program, vcl_var.Declaration, vcl_var.Expression]
):
    @override
    def translate_program(self, program: vcl_ast.Program) -> vcl_var.Program:
        if isinstance(program, vcl_ast.Main):
            return self.translate_Main(program)
        raise NotImplementedError(type(program).__name__)

    @abstractmethod
    def translate_Main(self, program: vcl_ast.Main) -> vcl_var.Program: ...

    @override
    def translate_declaration(
        self, declaration: vcl_ast.Declaration
    ) -> vcl_var.Declaration:
        if isinstance(declaration, vcl_ast.DefFunction):
            return self.translate_DefFunction(declaration)
        if isinstance(declaration, vcl_ast.DefPostulate):
            return self.translate_DefPostulate(declaration)
        raise NotImplementedError(type(declaration).__name__)

    @abstractmethod
    def translate_DefFunction(
        self, declaration: vcl_ast.DefFunction
    ) -> vcl_var.Declaration: ...

    @abstractmethod
    def translate_DefPostulate(
        self, declaration: vcl_ast.DefPostulate
    ) -> vcl_var.Declaration: ...

    @override
    def translate_expression(
        self, expression: vcl_ast.Expression
    ) -> vcl_var.Expression:
        if isinstance(expression, vcl_ast.App):
            return self.translate_App(expression)
        if isinstance(expression, vcl_ast.BoundVar):
            return self.translate_BoundVar(expression)
        if isinstance(expression, vcl_ast.Builtin):
            return self.translate_Builtin(expression)
        if isinstance(expression, vcl_ast.FreeVar):
            return self.translate_FreeVar(expression)
        if isinstance(expression, vcl_ast.Lam):
            return self.translate_Lam(expression)
        if isinstance(expression, vcl_ast.Let):
            return self.translate_Let(expression)
        if isinstance(expression, vcl_ast.PartialApp):
            return self.translate_PartialApp(expression)
        if isinstance(expression, vcl_ast.Pi):
            return self.translate_Pi(expression)
        if isinstance(expression, vcl_ast.Universe):
            return self.translate_Universe(expression)
        raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: vcl_ast.App) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoundVar(
        self, expression: vcl_ast.BoundVar
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Builtin(self, expression: vcl_ast.Builtin) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_FreeVar(self, expression: vcl_ast.FreeVar) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Lam(self, expression: vcl_ast.Lam) -> vcl_var.Expression: ...

    def translate_Let(self, expression: vcl_ast.Let) -> vcl_var.Expression:
        return self.translate_expression(
            vcl_ast.App(
                provenance=expression.provenance,
                function=vcl_ast.Lam(
                    provenance=expression.provenance,
                    binders=(expression.binder,),
                    body=expression.body,
                ),
                arguments=[expression.bound],
            )
        )

    @abstractmethod
    def translate_PartialApp(
        self, expression: vcl_ast.PartialApp
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Pi(self, expression: vcl_ast.Pi) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Universe(
        self, expression: vcl_ast.Universe
    ) -> vcl_var.Expression: ...
