from abc import ABCMeta, abstractmethod
from typing import Any, Generic, Iterator, Sequence, Tuple, Type, cast

from typing_extensions import TypeVar, override

from . import _ast as vcl
from ._functools import (
    Function1,
    Function2,
    Function3,
    Operator1,
    Operator2,
    Relation2,
    cons,
    foldRight,
)

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

_Bool = TypeVar("_Bool")
_Index = TypeVar("_Index")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")
_SupportsEq = TypeVar("_SupportsEq")

_S = TypeVar("_S")
_T = TypeVar("_T")


class Builtins(
    Generic[
        _Bool,
        _Index,
        _Nat,
        _Int,
        _Rat,
        _SupportsEq,
    ],
    metaclass=ABCMeta,
):
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

    @abstractmethod
    def AtVector(self) -> Function2[Sequence[_T], _Nat, _T]:
        ...

    @abstractmethod
    def Bool(self, value: bool) -> _Bool:
        ...

    @abstractmethod
    def BoolType(self) -> Type[_Bool]:
        ...

    def ConsList(self) -> Function2[_T, Sequence[_T], Sequence[_T]]:
        return cons

    def ConsVector(self) -> Function2[_T, Sequence[_T], Sequence[_T]]:
        return cons

    @abstractmethod
    def DivRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def Eq(self) -> Relation2[_SupportsEq, _Bool]:
        ...

    @abstractmethod
    def Exists(self) -> Function1[Function1[_T, _Bool], _Bool]:
        ...

    def FoldList(
        self,
    ) -> Function3[Function2[_S, _T, _T], _T, Sequence[_S], _T]:
        return foldRight

    def FoldVector(
        self,
    ) -> Function3[Function2[_S, _T, _T], _T, Sequence[_S], _T]:
        return foldRight

    @abstractmethod
    def Forall(self) -> Function1[Function1[_T, _Bool], _Bool]:
        ...

    @abstractmethod
    def GeIndex(self) -> Relation2[_Index, _Bool]:
        ...

    @abstractmethod
    def GeInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def GeNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def GeRat(self) -> Relation2[_Rat, _Bool]:
        ...

    @abstractmethod
    def GtIndex(self) -> Relation2[_Index, _Bool]:
        ...

    @abstractmethod
    def GtInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def GtNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def GtRat(self) -> Relation2[_Rat, _Bool]:
        ...

    @abstractmethod
    def If(self) -> Function3[_Bool, _T, _T, _T]:
        ...

    @abstractmethod
    def Implies(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def Index(self, value: int) -> _Index:
        ...

    @abstractmethod
    def IndexType(self) -> Type[_Index]:
        ...

    @abstractmethod
    def Indices(self) -> Function1[_Index, Sequence[_Index]]:
        ...

    @abstractmethod
    def Int(self, value: int) -> _Int:
        ...

    @abstractmethod
    def IntType(self) -> Type[_Int]:
        ...

    @abstractmethod
    def LeIndex(self) -> Relation2[_Index, _Bool]:
        ...

    @abstractmethod
    def LeInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def LeNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def LeRat(self) -> Relation2[_Rat, _Bool]:
        ...

    def ListType(self) -> Function1[Type[_T], Type[Sequence[_T]]]:
        return lambda T: Sequence[T]  # type: ignore[valid-type]

    @abstractmethod
    def LtIndex(self) -> Relation2[_Index, _Bool]:
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
    def Nat(self, value: int) -> _Nat:
        ...

    @abstractmethod
    def NatType(self) -> Type[_Nat]:
        ...

    @abstractmethod
    def Ne(self) -> Relation2[_SupportsEq, _Bool]:
        ...

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
    def PowRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def Rat(self, numerator: int, denomenator: int) -> _Rat:
        ...

    @abstractmethod
    def RatType(self) -> Type[_Rat]:
        ...

    @abstractmethod
    def SubInt(self) -> Operator2[_Int]:
        ...

    @abstractmethod
    def SubRat(self) -> Operator2[_Rat]:
        ...

    def Unit(self) -> Tuple[()]:
        return ()

    def UnitType(self) -> Type[Tuple[()]]:
        return cast(Type[Tuple[()]], Tuple[()])

    def Vector(self, values: Sequence[_T] = ()) -> Sequence[_T]:
        return tuple(values)

    def VectorType(self) -> Function2[Type[_T], int, Type[Sequence[_T]]]:
        return lambda T: lambda _i: Sequence[T]  # type: ignore[valid-type]


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
