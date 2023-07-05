from typing import Optional, Sequence, Type

from typing_extensions import TypeAlias, TypeVar

from . import BuiltinInterpreter
from ._functools import Function, Operator1, Operator2, Relation, foldRight

_S = TypeVar("_S")
_T = TypeVar("_T")

_AnyNum = TypeVar("_AnyNum", int, float)
_HasDiv: TypeAlias = float
_HasOrd = TypeVar("_HasOrd", int, float, bool)


class PythonBuiltinInterpreter(
    BuiltinInterpreter[
        bool,
        int,
        int,
        float,
        _AnyNum,
        _AnyNum,
        _AnyNum,
        _AnyNum,
        _HasDiv,
        _AnyNum,
        _AnyNum,
        _AnyNum,
        _HasOrd,
    ]
):
    def interpret_Add(self, _cls: Optional[Type[_AnyNum]] = None) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x + y

    def interpret_And(self) -> Operator2[bool]:
        return lambda x: lambda y: x and y

    def interpret_At(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[Sequence[_T], Function[int, _T]]:
        return lambda xs: lambda n: xs[n]

    def interpret_Bool(self, value: bool) -> bool:
        return value

    def interpret_BoolType(self) -> Type[bool]:
        return bool

    def interpret_Div(self, _cls: Optional[Type[_HasDiv]] = None) -> Operator2[_HasDiv]:
        return lambda x: lambda y: x / y

    def interpret_Eq(self, _cls: Optional[Type[_T]] = None) -> Relation[_T, bool]:
        return lambda x: lambda y: x == y

    def interpret_Exists(self) -> Function[Function[_T, bool], bool]:
        raise NotImplementedError("Exists is unsupported")

    def interpret_Fold(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[
        Function[_T, Function[_S, _S]], Function[_S, Function[Sequence[_T], _S]]
    ]:
        return lambda f: lambda x: lambda xs: foldRight(f, x, xs)

    def interpret_Forall(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[Function[_T, bool], bool]:
        raise NotImplementedError("Forall is unsupported")

    def interpret_Ge(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x >= y

    def interpret_Gt(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x > y

    def interpret_If(
        self, _cls: Optional[Type[_T]] = None
    ) -> Function[bool, Function[_T, Function[_T, _T]]]:
        return lambda i: lambda t: lambda e: t if i else e

    def interpret_Implies(self) -> Operator2[bool]:
        return lambda x: lambda y: (not x) or y

    def interpret_Index(self, value: int) -> int:
        return value

    def interpret_IndexType(self) -> Type[int]:
        return int

    def interpret_Indicator(self, _cls: Optional[Type[_T]] = None) -> Relation[_T, int]:
        return lambda x: lambda y: 1 if x == y else 0

    def interpret_Int(self, value: int) -> int:
        return value

    def interpret_IntType(self) -> Type[int]:
        return int

    def interpret_Indices(self) -> Function[int, Sequence[int]]:
        return lambda x: range(0, x)

    def interpret_Le(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x <= y

    def interpret_Lt(
        self, _cls: Optional[Type[_HasOrd]] = None
    ) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x < y

    def interpret_Max(self, _cls: Optional[Type[_AnyNum]] = None) -> Operator2[_AnyNum]:
        return lambda x: lambda y: max(x, y)

    def interpret_Min(self, _cls: Optional[Type[_AnyNum]] = None) -> Operator2[_AnyNum]:
        return lambda x: lambda y: min(x, y)

    def interpret_Mul(self, _cls: Optional[Type[_AnyNum]] = None) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x * y

    def interpret_Nat(self, value: int) -> int:
        return value

    def interpret_NatType(self) -> Type[int]:
        return int

    def interpret_Ne(self, _cls: Optional[Type[_T]] = None) -> Relation[_T, bool]:
        return lambda x: lambda y: x != y

    def interpret_Neg(self, _cls: Optional[Type[_AnyNum]] = None) -> Operator1[_AnyNum]:
        return lambda x: -x

    def interpret_Not(self) -> Operator1[bool]:
        return lambda x: not x

    def interpret_Or(self) -> Operator2[bool]:
        return lambda x: lambda y: x or y

    def interpret_Pow(self, _cls: Optional[Type[_AnyNum]] = None) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x**y

    def interpret_Rat(self, numerator: int, denominator: int) -> float:
        return numerator / denominator

    def interpret_RatType(self) -> Type[float]:
        return float

    def interpret_Sub(self, _cls: Optional[Type[_AnyNum]] = None) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x - y
