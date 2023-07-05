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

    @override
    def translate_builtin(self, builtin: Builtin) -> _Builtin:
        if isinstance(builtin, Add):
            return self.translate_Add(builtin)
        if isinstance(builtin, And):
            return self.translate_And(builtin)
        if isinstance(builtin, At):
            return self.translate_At(builtin)
        if isinstance(builtin, Bool):
            return self.translate_Bool(builtin)
        if isinstance(builtin, BoolType):
            return self.translate_BoolType(builtin)
        if isinstance(builtin, Cons):
            return self.translate_Cons(builtin)
        if isinstance(builtin, ConsVector):
            return self.translate_ConsVector(builtin)
        if isinstance(builtin, Div):
            return self.translate_Div(builtin)
        if isinstance(builtin, Eq):
            return self.translate_Eq(builtin)
        if isinstance(builtin, Exists):
            return self.translate_Exists(builtin)
        if isinstance(builtin, Fold):
            return self.translate_Fold(builtin)
        if isinstance(builtin, Forall):
            return self.translate_Forall(builtin)
        if isinstance(builtin, Ge):
            return self.translate_Ge(builtin)
        if isinstance(builtin, Gt):
            return self.translate_Gt(builtin)
        if isinstance(builtin, If):
            return self.translate_If(builtin)
        if isinstance(builtin, Implies):
            return self.translate_Implies(builtin)
        if isinstance(builtin, Index):
            return self.translate_Index(builtin)
        if isinstance(builtin, IndexType):
            return self.translate_IndexType(builtin)
        if isinstance(builtin, Indicator):
            return self.translate_Indicator(builtin)
        if isinstance(builtin, Indices):
            return self.translate_Indices(builtin)
        if isinstance(builtin, Int):
            return self.translate_Int(builtin)
        if isinstance(builtin, IntType):
            return self.translate_IntType(builtin)
        if isinstance(builtin, Le):
            return self.translate_Le(builtin)
        if isinstance(builtin, Lt):
            return self.translate_Lt(builtin)
        if isinstance(builtin, ListType):
            return self.translate_ListType(builtin)
        if isinstance(builtin, Max):
            return self.translate_Max(builtin)
        if isinstance(builtin, Min):
            return self.translate_Min(builtin)
        if isinstance(builtin, Mul):
            return self.translate_Mul(builtin)
        if isinstance(builtin, Nat):
            return self.translate_Nat(builtin)
        if isinstance(builtin, NatType):
            return self.translate_NatType(builtin)
        if isinstance(builtin, Ne):
            return self.translate_Ne(builtin)
        if isinstance(builtin, Neg):
            return self.translate_Neg(builtin)
        if isinstance(builtin, Nil):
            return self.translate_Nil(builtin)
        if isinstance(builtin, Not):
            return self.translate_Not(builtin)
        if isinstance(builtin, Or):
            return self.translate_Or(builtin)
        if isinstance(builtin, Pow):
            return self.translate_Pow(builtin)
        if isinstance(builtin, Rat):
            return self.translate_Rat(builtin)
        if isinstance(builtin, RatType):
            return self.translate_RatType(builtin)
        if isinstance(builtin, Sub):
            return self.translate_Sub(builtin)
        if isinstance(builtin, Unit):
            return self.translate_Unit(builtin)
        if isinstance(builtin, UnitType):
            return self.translate_UnitType(builtin)
        if isinstance(builtin, Vector):
            return self.translate_Vector(builtin)
        if isinstance(builtin, VectorType):
            return self.translate_VectorType(builtin)
        raise NotImplementedError(type(builtin).__name__)

    @abstractmethod
    def translate_Add(self, builtin: Add) -> _Builtin:
        ...

    @abstractmethod
    def translate_And(self, builtin: And) -> _Builtin:
        ...

    @abstractmethod
    def translate_At(self, builtin: At) -> _Builtin:
        ...

    @abstractmethod
    def translate_Bool(self, builtin: Bool) -> _Builtin:
        ...

    @abstractmethod
    def translate_BoolType(self, builtin: BoolType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Cons(self, builtin: Cons) -> _Builtin:
        ...

    @abstractmethod
    def translate_ConsVector(self, builtin: ConsVector) -> _Builtin:
        ...

    @abstractmethod
    def translate_Div(self, builtin: Div) -> _Builtin:
        ...

    @abstractmethod
    def translate_Eq(self, builtin: Eq) -> _Builtin:
        ...

    @abstractmethod
    def translate_Exists(self, builtin: Exists) -> _Builtin:
        ...

    @abstractmethod
    def translate_Fold(self, builtin: Fold) -> _Builtin:
        ...

    @abstractmethod
    def translate_Forall(self, builtin: Forall) -> _Builtin:
        ...

    @abstractmethod
    def translate_Ge(self, builtin: Ge) -> _Builtin:
        ...

    @abstractmethod
    def translate_Gt(self, builtin: Gt) -> _Builtin:
        ...

    @abstractmethod
    def translate_If(self, builtin: If) -> _Builtin:
        ...

    @abstractmethod
    def translate_Implies(self, builtin: Implies) -> _Builtin:
        ...

    @abstractmethod
    def translate_Index(self, builtin: Index) -> _Builtin:
        ...

    @abstractmethod
    def translate_IndexType(self, builtin: IndexType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Indicator(self, builtin: Indicator) -> _Builtin:
        ...

    @abstractmethod
    def translate_Indices(self, builtin: Indices) -> _Builtin:
        ...

    @abstractmethod
    def translate_Int(self, builtin: Int) -> _Builtin:
        ...

    @abstractmethod
    def translate_IntType(self, builtin: IntType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Le(self, builtin: Le) -> _Builtin:
        ...

    @abstractmethod
    def translate_Lt(self, builtin: Lt) -> _Builtin:
        ...

    @abstractmethod
    def translate_ListType(self, builtin: ListType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Max(self, builtin: Max) -> _Builtin:
        ...

    @abstractmethod
    def translate_Min(self, builtin: Min) -> _Builtin:
        ...

    @abstractmethod
    def translate_Mul(self, builtin: Mul) -> _Builtin:
        ...

    @abstractmethod
    def translate_Nat(self, builtin: Nat) -> _Builtin:
        ...

    @abstractmethod
    def translate_NatType(self, builtin: NatType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Ne(self, builtin: Ne) -> _Builtin:
        ...

    @abstractmethod
    def translate_Neg(self, builtin: Neg) -> _Builtin:
        ...

    @abstractmethod
    def translate_Nil(self, builtin: Nil) -> _Builtin:
        ...

    @abstractmethod
    def translate_Not(self, builtin: Not) -> _Builtin:
        ...

    @abstractmethod
    def translate_Or(self, builtin: Or) -> _Builtin:
        ...

    @abstractmethod
    def translate_Pow(self, builtin: Pow) -> _Builtin:
        ...

    @abstractmethod
    def translate_Rat(self, builtin: Rat) -> _Builtin:
        ...

    @abstractmethod
    def translate_RatType(self, builtin: RatType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Sub(self, builtin: Sub) -> _Builtin:
        ...

    @abstractmethod
    def translate_Unit(self, builtin: Unit) -> _Builtin:
        ...

    @abstractmethod
    def translate_UnitType(self, builtin: UnitType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Vector(self, builtin: Vector) -> _Builtin:
        ...

    @abstractmethod
    def translate_VectorType(self, builtin: VectorType) -> _Builtin:
        ...
