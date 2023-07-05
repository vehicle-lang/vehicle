from abc import ABCMeta, abstractmethod
from typing import Generic, Optional, Sequence, Tuple, Type, cast

from typing_extensions import TypeVar

from ._functools import Function, Operator1, Operator2, Relation

_Bool = TypeVar("_Bool")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")

_HasNeg = TypeVar("_HasNeg")
_HasAdd = TypeVar("_HasAdd")
_HasSub = TypeVar("_HasSub")
_HasMul = TypeVar("_HasMul")
_HasDiv = TypeVar("_HasDiv")
_HasMin = TypeVar("_HasMin")
_HasMax = TypeVar("_HasMax")
_HasPow = TypeVar("_HasPow")
_HasOrd = TypeVar("_HasOrd")

_S = TypeVar("_S")
_T = TypeVar("_T")


class BuiltinInterpreter(
    Generic[
        _Bool,
        _Nat,
        _Int,
        _Rat,
        _HasNeg,
        _HasAdd,
        _HasSub,
        _HasMul,
        _HasDiv,
        _HasMin,
        _HasMax,
        _HasPow,
        _HasOrd,
    ],
    metaclass=ABCMeta,
):
    @abstractmethod
    def interpret_Add(self, _cls: Optional[Type[_HasAdd]] = None) -> Operator2[_HasAdd]:
        ...

    @abstractmethod
    def interpret_And(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def interpret_At(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[Sequence[_T], Function[_Nat, _T]]:
        ...

    @abstractmethod
    def interpret_Bool(self, value: bool) -> _Bool:
        ...

    @abstractmethod
    def interpret_BoolType(self) -> Type[_Bool]:
        ...

    def interpret_Cons(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[_T, Function[Sequence[_T], Sequence[_T]]]:
        return lambda x: lambda xs: (x, *xs)

    def interpret_ConsVector(
        self,
    ) -> Function[_T, Function[Sequence[_T], Sequence[_T]]]:
        return lambda x: lambda xs: (x, *xs)

    @abstractmethod
    def interpret_Div(self, _cls: Optional[Type[_HasDiv]] = None) -> Operator2[_HasDiv]:
        ...

    @abstractmethod
    def interpret_Eq(self, _cls: Optional[Type[_T]] = None) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Exists(self) -> Function[Function[_T, _Bool], _Bool]:
        ...

    @abstractmethod
    def interpret_Fold(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[
        Function[_T, Function[_S, _S]], Function[_S, Function[Sequence[_T], _S]]
    ]:
        ...

    @abstractmethod
    def interpret_Forall(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[Function[_T, _Bool], _Bool]:
        ...

    @abstractmethod
    def interpret_Ge(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, _Bool]:
        ...

    @abstractmethod
    def interpret_Gt(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, _Bool]:
        ...

    @abstractmethod
    def interpret_If(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[_Bool, Function[_T, Function[_T, _T]]]:
        ...

    @abstractmethod
    def interpret_Implies(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def interpret_Index(self, value: int) -> _Nat:
        ...

    @abstractmethod
    def interpret_IndexType(self) -> Type[_Nat]:
        ...

    @abstractmethod
    def interpret_Indicator(self, cls: Type[_T]) -> Relation[_T, _Nat]:
        ...

    @abstractmethod
    def interpret_Indices(self) -> Function[_Nat, Sequence[_Nat]]:
        ...

    @abstractmethod
    def interpret_Int(self, value: int) -> _Int:
        ...

    @abstractmethod
    def interpret_IntType(self) -> Type[_Int]:
        ...

    def interpret_ListType(self) -> Function[Type[_T], Type[Sequence[_T]]]:
        return lambda cls: Sequence[cls]  # type: ignore[valid-type]

    @abstractmethod
    def interpret_Max(self, _cls: Optional[Type[_HasMax]] = None) -> Operator2[_HasMax]:
        ...

    @abstractmethod
    def interpret_Min(self, _cls: Optional[Type[_HasMin]] = None) -> Operator2[_HasMin]:
        ...

    @abstractmethod
    def interpret_Mul(self, _cls: Optional[Type[_HasMul]] = None) -> Operator2[_HasMul]:
        ...

    @abstractmethod
    def interpret_Ne(self, _cls: Optional[Type[_T]] = None) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Neg(self, _cls: Optional[Type[_HasNeg]] = None) -> Operator1[_HasNeg]:
        ...

    def interpret_Nil(self, _cls: Optional[Type[_T]] = None) -> Sequence[_T]:
        return ()

    @abstractmethod
    def interpret_Not(self) -> Operator1[_Bool]:
        ...

    @abstractmethod
    def interpret_Or(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def interpret_Pow(self, _cls: Optional[Type[_HasPow]] = None) -> Operator2[_HasPow]:
        ...

    @abstractmethod
    def interpret_Le(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, _Bool]:
        ...

    @abstractmethod
    def interpret_Lt(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, _Bool]:
        ...

    @abstractmethod
    def interpret_NatType(self) -> Type[_Nat]:
        ...

    @abstractmethod
    def interpret_Nat(self, value: int) -> _Nat:
        ...

    @abstractmethod
    def interpret_Rat(self, numerator: int, denominator: int) -> _Rat:
        ...

    @abstractmethod
    def interpret_RatType(self) -> Type[_Rat]:
        ...

    @abstractmethod
    def interpret_Sub(self, _cls: Optional[Type[_HasSub]] = None) -> Operator2[_HasSub]:
        ...

    def interpret_Unit(self) -> Tuple[()]:
        return ()

    def interpret_UnitType(self) -> Type[Tuple[()]]:
        return cast(Type[Tuple[()]], Tuple[()])

    def interpret_Vector(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[Sequence[_T], Sequence[_T]]:
        return lambda xs: tuple(xs)

    def interpret_VectorType(
        self,
    ) -> Function[_Nat, Function[Type[_T], Type[Sequence[_T]]]]:
        return lambda _n: lambda cls: Sequence[cls]  # type: ignore[valid-type]
