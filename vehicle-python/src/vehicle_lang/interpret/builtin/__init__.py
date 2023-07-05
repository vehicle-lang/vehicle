from abc import ABCMeta, abstractmethod
from typing import Any, Callable, Generic, Sequence, Tuple, Type, cast, overload

from typing_extensions import TypeAlias, TypeVar

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

_S = TypeVar("_S")
_T = TypeVar("_T")

Function: TypeAlias = Callable[[_S], _T]
Relation: TypeAlias = Function[_T, Function[_T, _Bool]]
UnaryOp: TypeAlias = Function[_T, _T]
BinaryOp: TypeAlias = Function[_T, Function[_T, _T]]


class InterpretBuiltin(
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
    ],
    metaclass=ABCMeta,
):
    def interpret_UnitType(self) -> Type[Tuple[()]]:
        return cast(Type[Tuple[()]], Tuple[()])

    def interpret_Unit(self) -> Tuple[()]:
        return ()

    @abstractmethod
    def interpret_BoolType(self) -> Type[_Bool]:
        ...

    @abstractmethod
    def interpret_Bool(self, value: bool) -> _Bool:
        ...

    @abstractmethod
    def interpret_If(
        self, cls: Type[_T]
    ) -> Function[_Bool, Function[_T, Function[_T, _T]]]:
        ...

    @abstractmethod
    def interpret_Not(self) -> UnaryOp[_Bool]:
        ...

    @abstractmethod
    def interpret_And(self) -> BinaryOp[_Bool]:
        ...

    @abstractmethod
    def interpret_Or(self) -> BinaryOp[_Bool]:
        ...

    @abstractmethod
    def interpret_Implies(self) -> BinaryOp[_Bool]:
        ...

    @abstractmethod
    def interpret_Forall(self, _cls: Type[_T]) -> Function[Function[_T, _Bool], _Bool]:
        ...

    @abstractmethod
    def interpret_Exists(self) -> Function[Function[_T, _Bool], _Bool]:
        ...

    @abstractmethod
    def interpret_Eq(self, _cls: Type[_T]) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Ne(self, _cls: Type[_T]) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Le(self, _cls: Type[_T]) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Lt(self, _cls: Type[_T]) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Ge(self, _cls: Type[_T]) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Gt(self, _cls: Type[_T]) -> Relation[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_IndexType(self) -> Type[_Nat]:
        ...

    @abstractmethod
    def interpret_Index(self, value: int) -> _Nat:
        ...

    @abstractmethod
    def interpret_Indices(self) -> Function[_Nat, Sequence[_Nat]]:
        ...

    @abstractmethod
    def interpret_NatType(self) -> Type[_Nat]:
        ...

    @abstractmethod
    def interpret_Nat(self, value: int) -> _Nat:
        ...

    @abstractmethod
    def interpret_IntType(self) -> Type[_Int]:
        ...

    @abstractmethod
    def interpret_Int(self, value: int) -> _Int:
        ...

    @abstractmethod
    def interpret_RatType(self) -> Type[_Rat]:
        ...

    @abstractmethod
    def interpret_Rat(self, numerator: int, denominator: int) -> _Rat:
        ...

    @abstractmethod
    def interpret_Neg(self, _cls: Type[_HasNeg]) -> UnaryOp[_HasNeg]:
        ...

    @abstractmethod
    def interpret_Add(self, _cls: Type[_HasAdd]) -> BinaryOp[_HasAdd]:
        ...

    @abstractmethod
    def interpret_Sub(self, _cls: Type[_HasSub]) -> BinaryOp[_HasSub]:
        ...

    @abstractmethod
    def interpret_Mul(self, _cls: Type[_HasMul]) -> BinaryOp[_HasMul]:
        ...

    @abstractmethod
    def interpret_Div(self, _cls: Type[_HasDiv]) -> BinaryOp[_HasDiv]:
        ...

    @abstractmethod
    def interpret_Min(self, _cls: Type[_HasMin]) -> BinaryOp[_HasMin]:
        ...

    @abstractmethod
    def interpret_Max(self, _cls: Type[_HasMax]) -> BinaryOp[_HasMax]:
        ...

    @abstractmethod
    def interpret_Pow(self, _cls: Type[_HasPow]) -> BinaryOp[_HasPow]:
        ...

    @abstractmethod
    def interpret_Indicator(self, cls: Type[_T]) -> BinaryOp[_T]:
        ...

    def interpret_ListType(self) -> Function[Type[_T], Type[Sequence[_T]]]:
        return lambda cls: Sequence[cls]  # type: ignore[valid-type]

    def interpret_Nil(self, _cls: Type[_T]) -> Sequence[_T]:
        return ()

    def interpret_Cons(
        self, _cls: Type[_T]
    ) -> Function[_T, Function[Sequence[_T], Sequence[_T]]]:
        return lambda x: lambda xs: (x, *xs)

    @abstractmethod
    def interpret_Fold(
        self, _cls: Type[_T]
    ) -> Function[
        Function[_T, Function[_S, _S]], Function[_S, Function[Sequence[_S], _T]]
    ]:
        ...

    def interpret_VectorType(
        self,
    ) -> Function[_Nat, Function[Type[_T], Type[Sequence[_T]]]]:
        return lambda _n: lambda cls: Sequence[cls]  # type: ignore[valid-type]

    def interpret_Vector(self, _cls: Type[_T]) -> Function[Sequence[_T], Sequence[_T]]:
        return lambda xs: tuple(xs)

    def interpret_ConsVector(
        self,
    ) -> Function[_T, Function[Sequence[_T], Sequence[_T]]]:
        return lambda x: lambda xs: (x, *xs)

    @abstractmethod
    def interpret_At(
        self, _cls: Type[_T]
    ) -> Function[Sequence[_T], Function[_Nat, _T]]:
        ...
