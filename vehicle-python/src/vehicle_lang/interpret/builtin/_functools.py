from functools import reduce
from typing import Callable, Sequence

from typing_extensions import TypeAlias, TypeVar

_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")

Function: TypeAlias = Callable[[_S], _T]
Relation: TypeAlias = Function[_S, Function[_S, _T]]

Operator1: TypeAlias = Function[_T, _T]
Operator2: TypeAlias = Function[_T, Function[_T, _T]]


def flip(f: Function[_S, Function[_T, _U]]) -> Function[_T, Function[_S, _U]]:
    return lambda y: lambda x: f(x)(y)


def uncurry(f: Function[_S, Function[_T, _U]]) -> Callable[[_S, _T], _U]:
    return lambda x, y: f(x)(y)


def foldRight(f: Function[_T, Function[_S, _S]], x: _S, xs: Sequence[_T]) -> _S:
    return reduce(uncurry(flip(f)), xs, initial=x)
