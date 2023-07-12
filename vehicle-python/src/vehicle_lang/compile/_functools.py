from functools import partial, reduce
from typing import Callable, Sequence, Union

from typing_extensions import TypeAlias, TypeVar

_R = TypeVar("_R")
_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")
_V = TypeVar("_V")

Function1: TypeAlias = Callable[[_S], _R]
Function2: TypeAlias = Callable[[_S, _T], _R]
Function3: TypeAlias = Callable[[_S, _T, _U], _R]
Relation2: TypeAlias = Function2[_S, _S, _T]
Operator1: TypeAlias = Function1[_T, _T]
Operator2: TypeAlias = Function2[_T, _T, _T]


# def flip(f: Function2[_S, _T, _U]) -> Function2[_T, _S, _U]:
#     return lambda y: lambda x: f(x)(y)


# def curry(f: Callable[[_S, _T], _U]) -> Function2[_S, _T, _U]:
#     return lambda x: partial(f, x)


# def uncurry(f: Function2[_S, _T, _U]) -> Callable[[_S, _T], _U]:
#     return lambda x, y: f(x)(y)


# def cons(x: _T) -> Function1[Sequence[_T], Sequence[_T]]:
#     return lambda xs: (x, *xs)


# def foldRight(f: Function2[_T, _S, _S]) -> Function2[_S, Sequence[_T], _S]:
#     _uncurry_f = uncurry(flip(f))
#     return lambda x: lambda xs: reduce(_uncurry_f, xs, x)
