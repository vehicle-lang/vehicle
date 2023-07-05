from abc import ABCMeta, abstractmethod
from typing import Generic

from typing_extensions import TypeVar, override

from .. import Binder, Builtin, Declaration, Expression, Program
from .._ast import (
    Add,
    And,
    App,
    At,
    Bool,
    BoolType,
    BoundVar,
    BuiltinOp,
    Cons,
    ConsVector,
    DefFunction,
    DefPostulate,
    Div,
    Eq,
    Exists,
    Fold,
    Forall,
    FreeVar,
    Ge,
    Gt,
    If,
    Implies,
    Index,
    IndexType,
    Indicator,
    Indices,
    Int,
    IntType,
    Lam,
    Le,
    Let,
    ListType,
    Lt,
    Main,
    Max,
    Min,
    Mul,
    Nat,
    NatType,
    Ne,
    Neg,
    Nil,
    Not,
    Or,
    Pi,
    Pow,
    Rat,
    RatType,
    Sub,
    Unit,
    UnitType,
    Universe,
    Vector,
    VectorType,
)

_Program = TypeVar("_Program")
_Declaration = TypeVar("_Declaration")
_Expression = TypeVar("_Expression")
_Binder = TypeVar("_Binder")
_Builtin = TypeVar("_Builtin")


class Translation(
    Generic[_Program, _Declaration, _Expression, _Binder, _Builtin], metaclass=ABCMeta
):
    @abstractmethod
    def translate_program(self, program: Program) -> _Program:
        ...

    @abstractmethod
    def translate_declaration(self, declaration: Declaration) -> _Declaration:
        ...

    @abstractmethod
    def translate_expression(self, expression: Expression) -> _Expression:
        ...

    @abstractmethod
    def translate_binder(self, binder: Binder) -> _Binder:
        ...

    @abstractmethod
    def translate_builtin(self, builtin: Builtin) -> _Builtin:
        ...


class ABCTranslation(
    Translation[_Program, _Declaration, _Expression, _Binder, _Builtin]
):
    @override
    def translate_program(self, program: Program) -> _Program:
        if isinstance(program, Main):
            return self.translate_Main(program)
        raise NotImplementedError(type(program).__name__)

    @abstractmethod
    def translate_Main(self, main: Main) -> _Program:
        ...

    @override
    def translate_declaration(self, declaration: Declaration) -> _Declaration:
        if isinstance(declaration, DefFunction):
            return self.translate_DefFunction(declaration)
        if isinstance(declaration, DefPostulate):
            return self.translate_DefPostulate(declaration)
        raise NotImplementedError(type(declaration).__name__)

    @abstractmethod
    def translate_DefPostulate(self, defPostulate: DefPostulate) -> _Declaration:
        ...

    @abstractmethod
    def translate_DefFunction(self, defFunction: DefFunction) -> _Declaration:
        ...

    @override
    def translate_expression(self, expression: Expression) -> _Expression:
        # NOTE: maintain alphabetic ordering
        if isinstance(expression, App):
            return self.translate_App(expression)
        if isinstance(expression, BoundVar):
            return self.translate_BoundVar(expression)
        if isinstance(expression, BuiltinOp):
            return self.translate_BuiltinOp(expression)
        if isinstance(expression, FreeVar):
            return self.translate_FreeVar(expression)
        if isinstance(expression, Lam):
            return self.translate_Lam(expression)
        if isinstance(expression, Let):
            return self.translate_Let(expression)
        if isinstance(expression, Pi):
            return self.translate_Pi(expression)
        if isinstance(expression, Universe):
            return self.translate_Universe(expression)
        raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: App) -> _Expression:
        ...

    @abstractmethod
    def translate_BoundVar(self, expression: BoundVar) -> _Expression:
        ...

    @abstractmethod
    def translate_BuiltinOp(self, expression: BuiltinOp) -> _Expression:
        ...

    @abstractmethod
    def translate_FreeVar(self, expression: FreeVar) -> _Expression:
        ...

    @abstractmethod
    def translate_Lam(self, expression: Lam) -> _Expression:
        ...

    @abstractmethod
    def translate_Let(self, expression: Let) -> _Expression:
        ...

    @abstractmethod
    def translate_Pi(self, expression: Pi) -> _Expression:
        ...

    @abstractmethod
    def translate_Universe(self, expression: Universe) -> _Expression:
        ...
