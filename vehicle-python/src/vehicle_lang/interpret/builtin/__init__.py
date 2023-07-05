from abc import ABCMeta, abstractmethod
from typing import Any, Generic, Sequence, Tuple, Type, cast

from typing_extensions import TypeVar

from .. import _ast as vcl
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

_Bool = TypeVar("_Bool")
_Index = TypeVar("_Index")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")

_S = TypeVar("_S")
_T = TypeVar("_T")


class BuiltinInterpreter(
    Generic[
        _Bool,
        _Index,
        _Nat,
        _Int,
        _Rat,
    ],
    metaclass=ABCMeta,
):
    @abstractmethod
    def interpret_AddInt(self) -> Operator2[_Int]:
        ...

    @abstractmethod
    def interpret_AddNat(self) -> Operator2[_Nat]:
        ...

    @abstractmethod
    def interpret_AddRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def interpret_And(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def interpret_AtVector(self) -> Function2[Sequence[_T], _Nat, _T]:
        ...

    @abstractmethod
    def interpret_Bool(self, value: bool) -> _Bool:
        ...

    @abstractmethod
    def interpret_BoolType(self) -> Type[_Bool]:
        ...

    def interpret_ConsList(self) -> Function2[_T, Sequence[_T], Sequence[_T]]:
        return cons

    def interpret_ConsVector(self) -> Function2[_T, Sequence[_T], Sequence[_T]]:
        return cons

    @abstractmethod
    def interpret_DivRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def interpret_Eq(self) -> Relation2[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_Exists(self) -> Function1[Function1[_T, _Bool], _Bool]:
        ...

    def interpret_FoldList(
        self,
    ) -> Function3[Function2[_S, _T, _T], _T, Sequence[_S], _T]:
        return foldRight

    def interpret_FoldVector(
        self,
    ) -> Function3[Function2[_S, _T, _T], _T, Sequence[_S], _T]:
        return foldRight

    @abstractmethod
    def interpret_Forall(self) -> Function1[Function1[_T, _Bool], _Bool]:
        ...

    @abstractmethod
    def interpret_GeIndex(self) -> Relation2[_Index, _Bool]:
        ...

    @abstractmethod
    def interpret_GeInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def interpret_GeNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def interpret_GeRat(self) -> Relation2[_Rat, _Bool]:
        ...

    @abstractmethod
    def interpret_GtIndex(self) -> Relation2[_Index, _Bool]:
        ...

    @abstractmethod
    def interpret_GtInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def interpret_GtNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def interpret_GtRat(self) -> Relation2[_Rat, _Bool]:
        ...

    @abstractmethod
    def interpret_If(self) -> Function3[_Bool, _T, _T, _T]:
        ...

    @abstractmethod
    def interpret_Implies(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def interpret_Index(self, value: int) -> _Index:
        ...

    @abstractmethod
    def interpret_IndexType(self) -> Type[_Index]:
        ...

    @abstractmethod
    def interpret_Indices(self) -> Function1[_Index, Sequence[_Index]]:
        ...

    @abstractmethod
    def interpret_Int(self, value: int) -> _Int:
        ...

    @abstractmethod
    def interpret_IntType(self) -> Type[_Int]:
        ...

    @abstractmethod
    def interpret_LeIndex(self) -> Relation2[_Index, _Bool]:
        ...

    @abstractmethod
    def interpret_LeInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def interpret_LeNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def interpret_LeRat(self) -> Relation2[_Rat, _Bool]:
        ...

    def interpret_ListType(self) -> Function1[Type[_T], Type[Sequence[_T]]]:
        return lambda T: Sequence[T]  # type: ignore[valid-type]

    @abstractmethod
    def interpret_LtIndex(self) -> Relation2[_Index, _Bool]:
        ...

    @abstractmethod
    def interpret_LtInt(self) -> Relation2[_Int, _Bool]:
        ...

    @abstractmethod
    def interpret_LtNat(self) -> Relation2[_Nat, _Bool]:
        ...

    @abstractmethod
    def interpret_LtRat(self) -> Relation2[_Rat, _Bool]:
        ...

    @abstractmethod
    def interpret_MaxRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def interpret_MinRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def interpret_MulInt(self) -> Operator2[_Int]:
        ...

    @abstractmethod
    def interpret_MulNat(self) -> Operator2[_Nat]:
        ...

    @abstractmethod
    def interpret_MulRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def interpret_Nat(self, value: int) -> _Nat:
        ...

    @abstractmethod
    def interpret_NatType(self) -> Type[_Nat]:
        ...

    @abstractmethod
    def interpret_Ne(self) -> Relation2[_T, _Bool]:
        ...

    @abstractmethod
    def interpret_NegInt(self) -> Operator1[_Int]:
        ...

    @abstractmethod
    def interpret_NegRat(self) -> Operator1[_Rat]:
        ...

    def interpret_NilList(self) -> Sequence[_T]:
        return ()

    @abstractmethod
    def interpret_Not(self) -> Operator1[_Bool]:
        ...

    @abstractmethod
    def interpret_Or(self) -> Operator2[_Bool]:
        ...

    @abstractmethod
    def interpret_PowRat(self) -> Operator2[_Rat]:
        ...

    @abstractmethod
    def interpret_Rat(self, numerator: int, denomenator: int) -> _Rat:
        ...

    @abstractmethod
    def interpret_RatType(self) -> Type[_Rat]:
        ...

    @abstractmethod
    def interpret_SubInt(self) -> Operator2[_Int]:
        ...

    @abstractmethod
    def interpret_SubRat(self) -> Operator2[_Rat]:
        ...

    def interpret_Unit(self) -> Tuple[()]:
        return ()

    def interpret_UnitType(self) -> Type[Tuple[()]]:
        return cast(Type[Tuple[()]], Tuple[()])

    def interpret_Vector(self) -> Function1[Sequence[_T], Sequence[_T]]:
        return tuple

    def interpret_VectorType(self) -> Function1[Type[_T], Type[Sequence[_T]]]:
        return lambda T: Sequence[T]  # type: ignore[valid-type]
