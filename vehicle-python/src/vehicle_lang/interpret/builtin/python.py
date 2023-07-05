from functools import reduce
from typing import Sequence, Type

from typing_extensions import TypeAlias, TypeVar

from . import Function, InterpretBuiltin, Operator1, Operator2, Relation

_S = TypeVar("_S")
_T = TypeVar("_T")

_AnyNum = TypeVar("_AnyNum", int, float)
_HasDiv: TypeAlias = float
_HasOrd = TypeVar("_HasOrd", int, float, bool)


def _foldr(f: Function[_T, Function[_S, _S]], x: _S, xs: Sequence[_T]) -> _S:
    """A definition of foldr in terms of reduce."""

    def _uncurry_f(x: _S, y: _T) -> _S:
        return f(y)(x)

    return reduce(_uncurry_f, xs, initial=x)


class PythonBuiltin(
    InterpretBuiltin[
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
    def interpret_BoolType(self) -> Type[bool]:
        return bool

    def interpret_Bool(self, value: bool) -> bool:
        return value

    def interpret_If(
        self, _cls: Type[_T]
    ) -> Function[bool, Function[_T, Function[_T, _T]]]:
        return lambda i: lambda t: lambda e: t if i else e

    def interpret_Not(self) -> Operator1[bool]:
        return lambda x: not x

    def interpret_And(self) -> Operator2[bool]:
        return lambda x: lambda y: x and y

    def interpret_Or(self) -> Operator2[bool]:
        return lambda x: lambda y: x or y

    def interpret_Implies(self) -> Operator2[bool]:
        return lambda x: lambda y: (not x) or y

    def interpret_Forall(self, _cls: Type[_T]) -> Function[Function[_T, bool], bool]:
        raise NotImplementedError("Forall is unsupported")

    def interpret_Exists(self) -> Function[Function[_T, bool], bool]:
        raise NotImplementedError("Exists is unsupported")

    def interpret_Eq(self, _cls: Type[_T]) -> Relation[_T, bool]:
        return lambda x: lambda y: x == y

    def interpret_Ne(self, _cls: Type[_T]) -> Relation[_T, bool]:
        return lambda x: lambda y: x != y

    def interpret_Le(self, _cls: Type[_HasOrd]) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x <= y

    def interpret_Lt(self, _cls: Type[_HasOrd]) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x < y

    def interpret_Ge(self, _cls: Type[_HasOrd]) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x >= y

    def interpret_Gt(self, _cls: Type[_HasOrd]) -> Relation[_HasOrd, bool]:
        return lambda x: lambda y: x > y

    def interpret_IndexType(self) -> Type[int]:
        return int

    def interpret_Index(self, value: int) -> int:
        return value

    def interpret_Indices(self) -> Function[int, Sequence[int]]:
        return lambda x: range(0, x)

    def interpret_NatType(self) -> Type[int]:
        return int

    def interpret_Nat(self, value: int) -> int:
        return value

    def interpret_IntType(self) -> Type[int]:
        return int

    def interpret_Int(self, value: int) -> int:
        return value

    def interpret_RatType(self) -> Type[float]:
        return float

    def interpret_Rat(self, numerator: int, denominator: int) -> float:
        return numerator / denominator

    def interpret_Neg(self, _cls: Type[_AnyNum]) -> Operator1[_AnyNum]:
        return lambda x: -x

    def interpret_Add(self, _cls: Type[_AnyNum]) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x + y

    def interpret_Sub(self, _cls: Type[_AnyNum]) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x - y

    def interpret_Mul(self, _cls: Type[_AnyNum]) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x * y

    def interpret_Div(self, cls: Type[_HasDiv]) -> Operator2[_HasDiv]:
        return lambda x: lambda y: x / y

    def interpret_Min(self, _cls: Type[_AnyNum]) -> Operator2[_AnyNum]:
        return lambda x: lambda y: min(x, y)

    def interpret_Max(self, _cls: Type[_AnyNum]) -> Operator2[_AnyNum]:
        return lambda x: lambda y: max(x, y)

    def interpret_Pow(self, _cls: Type[_AnyNum]) -> Operator2[_AnyNum]:
        return lambda x: lambda y: x**y

    def interpret_Indicator(self, _cls: Type[_T]) -> Relation[_T, int]:
        return lambda x: lambda y: 1 if x == y else 0

    def interpret_Fold(
        self, _cls: Type[_T]
    ) -> Function[
        Function[_T, Function[_S, _S]], Function[_S, Function[Sequence[_T], _S]]
    ]:
        return lambda f: lambda x: lambda xs: _foldr(f, x, xs)

    def interpret_At(self, _cls: Type[_T]) -> Function[Sequence[_T], Function[int, _T]]:
        return lambda xs: lambda n: xs[n]
