from abc import ABCMeta, abstractmethod
from typing import Any, Callable, Generic, Sequence, Tuple, Union

from typing_extensions import TypeAlias, TypeVar

from ._ast import (
    Add,
    And,
    At,
    Bool,
    BoolType,
    Cons,
    ConsVector,
    Div,
    Eq,
    Exists,
    Fold,
    Forall,
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
    Le,
    ListType,
    Lt,
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
    Power,
    Rat,
    RatType,
    Sub,
    Unit,
    UnitType,
    Vector,
    VectorType,
)

_Bool = TypeVar("_Bool")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")

_Unit: TypeAlias = Tuple[()]
_Value: TypeAlias = Union[
    _Unit, _Bool, _Nat, _Int, _Rat, Sequence["_Value"], Callable[["_Value"], "_Value"]
]


class InterpretBuiltin(Generic[_Bool, _Nat, _Int, _Rat], metaclass=ABCMeta):
    @abstractmethod
    def interpret_Nil(self, nil: Nil) -> Sequence[_Value]:
        ...

    @abstractmethod
    def interpret_Cons(
        self, cons: Cons
    ) -> Callable[[_Value], Callable[[Sequence[_Value]], Sequence[_Value]]]:
        ...

    @abstractmethod
    def interpret_Unit(self, unit: Unit) -> _Unit:
        ...

    @abstractmethod
    def interpret_Bool(self, bool: Bool) -> _Bool:
        ...

    @abstractmethod
    def interpret_Index(self, index: Index) -> _Nat:
        ...

    @abstractmethod
    def interpret_Nat(self, nat: Nat) -> _Nat:
        ...

    @abstractmethod
    def interpret_Int(self, int: Int) -> _Int:
        ...

    @abstractmethod
    def interpret_Rat(self, rat: Rat) -> _Rat:
        ...

    @abstractmethod
    def interpret_Vector(
        self, vector: Vector
    ) -> Callable[[Sequence[_Value]], Sequence[_Value]]:
        ...

    @abstractmethod
    def interpret_Not(self, notBuiltin: Not) -> Callable[[_Bool], _Bool]:
        ...

    @abstractmethod
    def interpret_And(
        self, andBuiltin: And
    ) -> Callable[[_Bool], Callable[[_Bool], _Bool]]:
        ...

    @abstractmethod
    def interpret_Or(
        self, orBuiltin: Or
    ) -> Callable[[_Bool], Callable[[_Bool], _Bool]]:
        ...

    @abstractmethod
    def interpret_Implies(
        self, implies: Implies
    ) -> Callable[[_Bool], Callable[[_Bool], _Bool]]:
        ...

    @abstractmethod
    def interpret_Forall(
        self, forall: Forall
    ) -> Callable[[Callable[[_Value], _Bool]], _Bool]:
        ...

    @abstractmethod
    def interpret_Exists(
        self, exists: Exists
    ) -> Callable[[Callable[[_Value], _Bool]], _Bool]:
        ...

    @abstractmethod
    def interpret_If(self, ifBuiltin: If):
        ...

    @abstractmethod
    def interpret_Neg(self, neg: Neg):
        ...

    @abstractmethod
    def interpret_Add(self, add: Add):
        ...

    @abstractmethod
    def interpret_Sub(self, sub: Sub):
        ...

    @abstractmethod
    def interpret_Mul(self, mul: Mul):
        ...

    @abstractmethod
    def interpret_Div(self, div: Div):
        ...

    @abstractmethod
    def interpret_Eq(self, eq: Eq):
        ...

    @abstractmethod
    def interpret_Ne(self, ne: Ne):
        ...

    @abstractmethod
    def interpret_Le(self, le: Le):
        ...

    @abstractmethod
    def interpret_Lt(self, lt: Lt):
        ...

    @abstractmethod
    def interpret_Ge(self, ge: Ge):
        ...

    @abstractmethod
    def interpret_Gt(self, gt: Gt):
        ...

    @abstractmethod
    def interpret_At(self, at: At):
        ...

    @abstractmethod
    def interpret_ConsVector(self, consVector: ConsVector):
        ...

    @abstractmethod
    def interpret_Fold(self, fold: Fold):
        ...

    @abstractmethod
    def interpret_Indices(self, indices: Indices):
        ...

    @abstractmethod
    def interpret_UnitType(self, unitType: UnitType):
        ...

    @abstractmethod
    def interpret_BoolType(self, boolType: BoolType):
        ...

    @abstractmethod
    def interpret_IndexType(self, indexType: IndexType):
        ...

    @abstractmethod
    def interpret_NatType(self, natType: NatType):
        ...

    @abstractmethod
    def interpret_IntType(self, intType: IntType):
        ...

    @abstractmethod
    def interpret_RatType(self, ratType: RatType):
        ...

    @abstractmethod
    def interpret_ListType(self, listType: ListType):
        ...

    @abstractmethod
    def interpret_VectorType(self, vectorType: VectorType):
        ...

    @abstractmethod
    def interpret_Min(self, min: Min):
        ...

    @abstractmethod
    def interpret_Max(self, max: Max):
        ...

    @abstractmethod
    def interpret_Power(self, power: Power):
        ...

    @abstractmethod
    def interpret_Indicator(self, indicator: Indicator):
        ...
