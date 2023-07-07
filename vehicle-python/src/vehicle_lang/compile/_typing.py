from typing_extensions import Protocol, Self


class SupportsAdd(Protocol):
    def __add__(self, other: Self) -> Self:
        ...


class SupportsSub(Protocol):
    def __sub__(self, other: Self) -> Self:
        ...


class SupportsMul(Protocol):
    def __mul__(self, other: Self) -> Self:
        ...


class SupportsDiv(Protocol):
    def __div__(self, other: Self) -> Self:
        ...
