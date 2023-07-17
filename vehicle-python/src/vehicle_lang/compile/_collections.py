from typing import Iterable

from typing_extensions import Protocol, TypeVar, runtime_checkable

_T_co = TypeVar("_T_co", covariant=True)


@runtime_checkable
class SupportsList(Iterable[_T_co], Protocol[_T_co]):
    pass


@runtime_checkable
class Subscriptable(Protocol[_T_co]):
    def __getitem__(self, index: int) -> _T_co:
        ...


@runtime_checkable
class SupportsVector(Iterable[_T_co], Subscriptable[_T_co], Protocol[_T_co]):
    pass
