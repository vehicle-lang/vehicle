import sys
from abc import ABCMeta, abstractmethod
from typing import Any, Generic, MutableSequence, Sequence, Tuple, Type, Union, cast

from typing_extensions import TypeAlias, TypeVar

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


class Builtins(
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
    def Eq(self) -> Relation2[_T, _Bool]:
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
    def Ne(self) -> Relation2[_T, _Bool]:
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

    def Vector(self, values: Sequence[_T]) -> Sequence[_T]:
        return tuple(values)

    def VectorType(self) -> Function2[Type[_T], int, Type[Sequence[_T]]]:
        return lambda T: lambda _i: Sequence[T]  # type: ignore[valid-type]
