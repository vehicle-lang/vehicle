import operator
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from functools import partial
from typing import (
    Any,
    Callable,
    Dict,
    Generic,
    Iterator,
    Sequence,
    SupportsFloat,
    SupportsInt,
    Tuple,
    cast,
)

from typing_extensions import TypeAlias, TypeVar, override

from .. import ast as vcl
from ._functools import (
    Function1,
    Function2,
    Function3,
    Operator1,
    Operator2,
    Relation2,
    cons,
    curry,
    foldRight,
)

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

_Bool = TypeVar("_Bool")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")

_S = TypeVar("_S")
_T = TypeVar("_T")


@dataclass(frozen=True)
class UnsupportedBuiltin(Exception):
    builtin: vcl.Builtin


Sampler: TypeAlias = Callable[[Dict[str, Any]], Iterator[Any]]


@dataclass(frozen=True, init=False)
class Builtins(
    Generic[
        _Bool,
        _Nat,
        _Int,
        _Rat,
    ],
    metaclass=ABCMeta,
):
    samplers: Dict[str, Sampler] = field(default_factory=dict)

    @abstractmethod
    def AddInt(self) -> Operator2[_Int]:
        ...

    @abstractmethod
    def AddNat(self) -> Operator2[_Nat]:
        ...

    @abstractmethod
    def AddRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def And(self) -> Operator2[_Bool]:
        ...

    def AtVector(self) -> Function2[Sequence[_T], int, _T]:
        return curry(operator.getitem)

    @abstractmethod
    def Bool(self, value: bool) -> _Bool:
        ...

    def ConsList(self) -> Function2[_T, Sequence[_T], Sequence[_T]]:
        return cons

    def ConsVector(self) -> Function2[_T, Sequence[_T], Sequence[_T]]:
        return cons

    @abstractmethod
    def DivRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def EqIndex(self) -> Relation2[int, _Bool]:
        ...

    @abstractmethod
    def EqInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def EqNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def EqRat(self) -> Relation2[_Rat, _Bool]:
        ...

    def Exists(
        self, name: str, context: Dict[str, Any]
    ) -> Function1[Function1[_T, _Bool], _Bool]:
        def _ExistsSample(predicate: Function1[_T, _Bool]) -> _Bool:
            return foldRight(self.Or())(self.Bool(False))(
                [predicate(cast(_T, sample)) for sample in self.Sample(name, context)]
            )

        return _ExistsSample

    def FoldList(
        self,
    ) -> Function3[Function2[_S, _T, _T], _T, Sequence[_S], _T]:
        return foldRight

    def FoldVector(
        self,
    ) -> Function3[Function2[_S, _T, _T], _T, Sequence[_S], _T]:
        return foldRight

    def Forall(
        self, name: str, context: Dict[str, Any]
    ) -> Function1[Function1[_T, _Bool], _Bool]:
        def _ForallSample(predicate: Function1[_T, _Bool]) -> _Bool:
            return foldRight(self.And())(self.Bool(True))(
                [predicate(cast(_T, sample)) for sample in self.Sample(name, context)]
            )

        return _ForallSample

    def GeIndex(self) -> Relation2[int, _Bool]:
        return lambda x: lambda y: self.Not()(self.LtIndex()(x)(y))

    def GeInt(self) -> Relation2[_Int, _Bool]:
        return lambda x: lambda y: self.Not()(self.LtInt()(x)(y))

    def GeNat(self) -> Relation2[_Nat, _Bool]:
        return lambda x: lambda y: self.Not()(self.LtNat()(x)(y))

    def GeRat(self) -> Relation2[_Rat, _Bool]:
        return lambda x: lambda y: self.Not()(self.LtRat()(x)(y))

    def GtIndex(self) -> Relation2[int, _Bool]:
        return lambda x: lambda y: self.Not()(self.LeIndex()(x)(y))

    def GtInt(self) -> Relation2[_Int, _Bool]:
        return lambda x: lambda y: self.Not()(self.LeInt()(x)(y))

    def GtNat(self) -> Relation2[_Nat, _Bool]:
        return lambda x: lambda y: self.Not()(self.LeNat()(x)(y))

    def GtRat(self) -> Relation2[_Rat, _Bool]:
        return lambda x: lambda y: self.Not()(self.LeRat()(x)(y))

    @abstractmethod
    def If(self) -> Function3[_Bool, _T, _T, _T]:
        ...

    def Implies(self) -> Operator2[_Bool]:
        return lambda x: lambda y: self.Or()(self.Not()(x))(y)

    def Index(self, value: SupportsInt) -> int:
        return value.__int__()

    def Indices(self) -> Function1[int, Sequence[int]]:
        return partial(range, 0)

    @abstractmethod
    def Int(self, value: SupportsInt) -> _Int:
        ...

    def LeIndex(self) -> Relation2[int, _Bool]:
        return lambda x: lambda y: self.Or()(self.EqIndex()(x)(y))(self.LtIndex()(x)(y))

    def LeInt(self) -> Relation2[_Int, _Bool]:
        return lambda x: lambda y: self.Or()(self.EqInt()(x)(y))(self.LtInt()(x)(y))

    def LeNat(self) -> Relation2[_Nat, _Bool]:
        return lambda x: lambda y: self.Or()(self.EqNat()(x)(y))(self.LtNat()(x)(y))

    def LeRat(self) -> Relation2[_Rat, _Bool]:
        return lambda x: lambda y: self.Or()(self.EqRat()(x)(y))(self.LtRat()(x)(y))

    @abstractmethod
    def LtIndex(self) -> Relation2[int, _Bool]:
        ...

    @abstractmethod
    def LtInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def LtNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def LtRat(self) -> Relation2[_Rat, _Bool]:
        ...

    @abstractmethod
    def MaxRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def MinRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def MulInt(self) -> Operator2[_Int]:
        ...

    @abstractmethod
    def MulNat(self) -> Operator2[_Nat]:
        ...

    @abstractmethod
    def MulRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def Nat(self, value: SupportsInt) -> _Nat:
        ...

    def NeIndex(self) -> Relation2[int, _Bool]:
        return lambda x: lambda y: self.Not()(self.EqIndex()(x)(y))

    def NeInt(self) -> Relation2[_Int, _Bool]:
        return lambda x: lambda y: self.Not()(self.EqInt()(x)(y))

    def NeNat(self) -> Relation2[_Nat, _Bool]:
        return lambda x: lambda y: self.Not()(self.EqNat()(x)(y))

    def NeRat(self) -> Relation2[_Rat, _Bool]:
        return lambda x: lambda y: self.Not()(self.EqRat()(x)(y))

    @abstractmethod
    def NegInt(self) -> Operator1[_Int]:
        ...

    @abstractmethod
    def NegRat(self) -> Operator1[_Rat]:
        ...

    def NilList(self) -> Sequence[_T]:
        return ()

    @abstractmethod
    def Not(self) -> Operator1[_Bool]:
        ...

    @abstractmethod
    def Or(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def PowRat(self) -> Function2[_Rat, _Int, _Rat]:
        ...

    @abstractmethod
    def Rat(self, value: SupportsFloat) -> _Rat:
        ...

    def Sample(self, name: str, context: Dict[str, Iterator[Any]]) -> Iterator[Any]:
        if name in self.samplers:
            return self.samplers[name](context)
        else:
            raise TypeError(f"Could not find sampler for '{name}'.")

    @abstractmethod
    def SubInt(self) -> Operator2[_Int]:
        ...

    @abstractmethod
    def SubRat(self) -> Operator2[_Rat]:
        ...

    def Unit(self) -> Tuple[()]:
        return ()

    def Vector(self, values: Sequence[_T] = ()) -> Sequence[_T]:
        return tuple(values)


AnyBuiltins: TypeAlias = Builtins[Any, Any, Any, Any]

################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


_Program = TypeVar("_Program")
_Declaration = TypeVar("_Declaration")
_Expression = TypeVar("_Expression")


class Translation(Generic[_Program, _Declaration, _Expression], metaclass=ABCMeta):
    @abstractmethod
    def translate_program(self, program: vcl.Program) -> _Program:
        ...

    @abstractmethod
    def translate_declaration(self, declaration: vcl.Declaration) -> _Declaration:
        ...

    @abstractmethod
    def translate_expression(self, expression: vcl.Expression) -> _Expression:
        ...


class ABCTranslation(Translation[_Program, _Declaration, _Expression]):
    @override
    def translate_program(self, program: vcl.Program) -> _Program:
        if isinstance(program, vcl.Main):
            return self.translate_Main(program)
        raise NotImplementedError(type(program).__name__)

    @abstractmethod
    def translate_Main(self, program: vcl.Main) -> _Program:
        ...

    @override
    def translate_declaration(self, declaration: vcl.Declaration) -> _Declaration:
        if isinstance(declaration, vcl.DefFunction):
            return self.translate_DefFunction(declaration)
        if isinstance(declaration, vcl.DefPostulate):
            return self.translate_DefPostulate(declaration)
        raise NotImplementedError(type(declaration).__name__)

    @abstractmethod
    def translate_DefPostulate(self, declaration: vcl.DefPostulate) -> _Declaration:
        ...

    @abstractmethod
    def translate_DefFunction(self, declaration: vcl.DefFunction) -> _Declaration:
        ...

    @override
    def translate_expression(self, expression: vcl.Expression) -> _Expression:
        if isinstance(expression, vcl.App):
            return self.translate_App(expression)
        if isinstance(expression, vcl.BoundVar):
            return self.translate_BoundVar(expression)
        if isinstance(expression, vcl.BuiltinOp):
            return self.translate_BuiltinOp(expression)
        if isinstance(expression, vcl.FreeVar):
            return self.translate_FreeVar(expression)
        if isinstance(expression, vcl.Lam):
            return self.translate_Lam(expression)
        if isinstance(expression, vcl.Let):
            return self.translate_Let(expression)
        if isinstance(expression, vcl.Pi):
            return self.translate_Pi(expression)
        if isinstance(expression, vcl.Universe):
            return self.translate_Universe(expression)
        raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: vcl.App) -> _Expression:
        ...

    @abstractmethod
    def translate_BoundVar(self, expression: vcl.BoundVar) -> _Expression:
        ...

    @abstractmethod
    def translate_BuiltinOp(self, expression: vcl.BuiltinOp) -> _Expression:
        ...

    @abstractmethod
    def translate_FreeVar(self, expression: vcl.FreeVar) -> _Expression:
        ...

    @abstractmethod
    def translate_Lam(self, expression: vcl.Lam) -> _Expression:
        ...

    def translate_Let(self, expression: vcl.Let) -> _Expression:
        return self.translate_expression(
            vcl.App(
                provenance=expression.provenance,
                func=vcl.Lam(
                    provenance=expression.provenance,
                    binder=expression.binder,
                    body=expression.body,
                ),
                args=[expression.bound],
            )
        )

    @abstractmethod
    def translate_Pi(self, expression: vcl.Pi) -> _Expression:
        ...

    @abstractmethod
    def translate_Universe(self, expression: vcl.Universe) -> _Expression:
        ...
