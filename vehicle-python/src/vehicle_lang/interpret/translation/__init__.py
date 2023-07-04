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
    Power,
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

    @override
    def translate_declaration(self, declaration: Declaration) -> _Declaration:
        if isinstance(declaration, DefFunction):
            return self.translate_DefFunction(declaration)
        if isinstance(declaration, DefPostulate):
            return self.translate_DefPostulate(declaration)
        raise NotImplementedError(type(declaration).__name__)

    @override
    def translate_expression(self, expression: Expression) -> _Expression:
        if isinstance(expression, Universe):
            return self.translate_Universe(expression)
        if isinstance(expression, App):
            return self.translate_App(expression)
        if isinstance(expression, Pi):
            return self.translate_Pi(expression)
        if isinstance(expression, BuiltinOp):
            return self.translate_BuiltinOp(expression)
        if isinstance(expression, BoundVar):
            return self.translate_BoundVar(expression)
        if isinstance(expression, FreeVar):
            return self.translate_FreeVar(expression)
        if isinstance(expression, Let):
            return self.translate_Let(expression)
        if isinstance(expression, Lam):
            return self.translate_Lam(expression)
        raise NotImplementedError(type(expression).__name__)

    @override
    def translate_builtin(self, builtin: Builtin) -> _Builtin:
        if isinstance(builtin, Nil):
            return self.translate_Nil(builtin)
        if isinstance(builtin, Cons):
            return self.translate_Cons(builtin)
        if isinstance(builtin, Unit):
            return self.translate_Unit(builtin)
        if isinstance(builtin, Bool):
            return self.translate_Bool(builtin)
        if isinstance(builtin, Index):
            return self.translate_Index(builtin)
        if isinstance(builtin, Nat):
            return self.translate_Nat(builtin)
        if isinstance(builtin, Int):
            return self.translate_Int(builtin)
        if isinstance(builtin, Rat):
            return self.translate_Rat(builtin)
        if isinstance(builtin, Vector):
            return self.translate_Vector(builtin)
        if isinstance(builtin, Not):
            return self.translate_Not(builtin)
        if isinstance(builtin, And):
            return self.translate_And(builtin)
        if isinstance(builtin, Or):
            return self.translate_Or(builtin)
        if isinstance(builtin, Implies):
            return self.translate_Implies(builtin)
        if isinstance(builtin, Forall):
            return self.translate_Forall(builtin)
        if isinstance(builtin, Exists):
            return self.translate_Exists(builtin)
        if isinstance(builtin, If):
            return self.translate_If(builtin)
        if isinstance(builtin, Neg):
            return self.translate_Neg(builtin)
        if isinstance(builtin, Add):
            return self.translate_Add(builtin)
        if isinstance(builtin, Sub):
            return self.translate_Sub(builtin)
        if isinstance(builtin, Mul):
            return self.translate_Mul(builtin)
        if isinstance(builtin, Div):
            return self.translate_Div(builtin)
        if isinstance(builtin, Eq):
            return self.translate_Eq(builtin)
        if isinstance(builtin, Ne):
            return self.translate_Ne(builtin)
        if isinstance(builtin, Le):
            return self.translate_Le(builtin)
        if isinstance(builtin, Lt):
            return self.translate_Lt(builtin)
        if isinstance(builtin, Ge):
            return self.translate_Ge(builtin)
        if isinstance(builtin, Gt):
            return self.translate_Gt(builtin)
        if isinstance(builtin, At):
            return self.translate_At(builtin)
        if isinstance(builtin, ConsVector):
            return self.translate_ConsVector(builtin)
        if isinstance(builtin, Fold):
            return self.translate_Fold(builtin)
        if isinstance(builtin, Indices):
            return self.translate_Indices(builtin)
        if isinstance(builtin, UnitType):
            return self.translate_UnitType(builtin)
        if isinstance(builtin, BoolType):
            return self.translate_BoolType(builtin)
        if isinstance(builtin, IndexType):
            return self.translate_IndexType(builtin)
        if isinstance(builtin, NatType):
            return self.translate_NatType(builtin)
        if isinstance(builtin, IntType):
            return self.translate_IntType(builtin)
        if isinstance(builtin, RatType):
            return self.translate_RatType(builtin)
        if isinstance(builtin, ListType):
            return self.translate_ListType(builtin)
        if isinstance(builtin, VectorType):
            return self.translate_VectorType(builtin)
        if isinstance(builtin, Min):
            return self.translate_Min(builtin)
        if isinstance(builtin, Max):
            return self.translate_Max(builtin)
        if isinstance(builtin, Power):
            return self.translate_Power(builtin)
        if isinstance(builtin, Indicator):
            return self.translate_Indicator(builtin)
        raise NotImplementedError(type(builtin).__name__)

    @abstractmethod
    def translate_Main(self, main: Main) -> _Program:
        ...

    @abstractmethod
    def translate_DefPostulate(self, defPostulate: DefPostulate) -> _Declaration:
        ...

    @abstractmethod
    def translate_DefFunction(self, defFunction: DefFunction) -> _Declaration:
        ...

    @abstractmethod
    def translate_Universe(self, universe: Universe) -> _Expression:
        ...

    @abstractmethod
    def translate_App(self, app: App) -> _Expression:
        ...

    @abstractmethod
    def translate_Pi(self, pi: Pi) -> _Expression:
        ...

    @abstractmethod
    def translate_BuiltinOp(self, builtinOp: BuiltinOp) -> _Expression:
        ...

    @abstractmethod
    def translate_BoundVar(self, boundVar: BoundVar) -> _Expression:
        ...

    @abstractmethod
    def translate_FreeVar(self, freeVar: FreeVar) -> _Expression:
        ...

    @abstractmethod
    def translate_Let(self, let: Let) -> _Expression:
        ...

    @abstractmethod
    def translate_Lam(self, lam: Lam) -> _Expression:
        ...

    @abstractmethod
    def translate_Nil(self, nil: Nil) -> _Builtin:
        ...

    @abstractmethod
    def translate_Cons(self, cons: Cons) -> _Builtin:
        ...

    @abstractmethod
    def translate_Unit(self, unit: Unit) -> _Builtin:
        ...

    @abstractmethod
    def translate_Bool(self, bool: Bool) -> _Builtin:
        ...

    @abstractmethod
    def translate_Index(self, index: Index) -> _Builtin:
        ...

    @abstractmethod
    def translate_Nat(self, nat: Nat) -> _Builtin:
        ...

    @abstractmethod
    def translate_Int(self, int: Int) -> _Builtin:
        ...

    @abstractmethod
    def translate_Rat(self, rat: Rat) -> _Builtin:
        ...

    @abstractmethod
    def translate_Vector(self, vector: Vector) -> _Builtin:
        ...

    @abstractmethod
    def translate_Not(self, notBuiltin: Not) -> _Builtin:
        ...

    @abstractmethod
    def translate_And(self, andBuiltin: And) -> _Builtin:
        ...

    @abstractmethod
    def translate_Or(self, orBuiltin: Or) -> _Builtin:
        ...

    @abstractmethod
    def translate_Implies(self, implies: Implies) -> _Builtin:
        ...

    @abstractmethod
    def translate_Forall(self, forall: Forall) -> _Builtin:
        ...

    @abstractmethod
    def translate_Exists(self, exists: Exists) -> _Builtin:
        ...

    @abstractmethod
    def translate_If(self, ifBuiltin: If) -> _Builtin:
        ...

    @abstractmethod
    def translate_Neg(self, neg: Neg) -> _Builtin:
        ...

    @abstractmethod
    def translate_Add(self, add: Add) -> _Builtin:
        ...

    @abstractmethod
    def translate_Sub(self, sub: Sub) -> _Builtin:
        ...

    @abstractmethod
    def translate_Mul(self, mul: Mul) -> _Builtin:
        ...

    @abstractmethod
    def translate_Div(self, div: Div) -> _Builtin:
        ...

    @abstractmethod
    def translate_Eq(self, eq: Eq) -> _Builtin:
        ...

    @abstractmethod
    def translate_Ne(self, ne: Ne) -> _Builtin:
        ...

    @abstractmethod
    def translate_Le(self, le: Le) -> _Builtin:
        ...

    @abstractmethod
    def translate_Lt(self, lt: Lt) -> _Builtin:
        ...

    @abstractmethod
    def translate_Ge(self, ge: Ge) -> _Builtin:
        ...

    @abstractmethod
    def translate_Gt(self, gt: Gt) -> _Builtin:
        ...

    @abstractmethod
    def translate_At(self, at: At) -> _Builtin:
        ...

    @abstractmethod
    def translate_ConsVector(self, consVector: ConsVector) -> _Builtin:
        ...

    @abstractmethod
    def translate_Fold(self, fold: Fold) -> _Builtin:
        ...

    @abstractmethod
    def translate_Indices(self, indices: Indices) -> _Builtin:
        ...

    @abstractmethod
    def translate_UnitType(self, unitType: UnitType) -> _Builtin:
        ...

    @abstractmethod
    def translate_BoolType(self, boolType: BoolType) -> _Builtin:
        ...

    @abstractmethod
    def translate_IndexType(self, indexType: IndexType) -> _Builtin:
        ...

    @abstractmethod
    def translate_NatType(self, natType: NatType) -> _Builtin:
        ...

    @abstractmethod
    def translate_IntType(self, intType: IntType) -> _Builtin:
        ...

    @abstractmethod
    def translate_RatType(self, ratType: RatType) -> _Builtin:
        ...

    @abstractmethod
    def translate_ListType(self, listType: ListType) -> _Builtin:
        ...

    @abstractmethod
    def translate_VectorType(self, vectorType: VectorType) -> _Builtin:
        ...

    @abstractmethod
    def translate_Min(self, min: Min) -> _Builtin:
        ...

    @abstractmethod
    def translate_Max(self, max: Max) -> _Builtin:
        ...

    @abstractmethod
    def translate_Power(self, power: Power) -> _Builtin:
        ...

    @abstractmethod
    def translate_Indicator(self, indicator: Indicator) -> _Builtin:
        ...
