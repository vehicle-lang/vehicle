from typing_extensions import Protocol, Self


class SupportsDunderLT(Protocol):
    def __lt__(self, other: Self) -> bool:
        ...


class SupportsDunderGT(Protocol):
    def __gt__(self, other: Self) -> bool:
        ...


class SupportsDunderLE(Protocol):
    def __le__(self, other: Self) -> bool:
        ...


class SupportsDunderGE(Protocol):
    def __ge__(self, other: Self) -> bool:
        ...


class SupportsRichComparison(SupportsDunderLT, SupportsDunderGT, Protocol):
    ...


class SupportsAbs(Protocol):
    def __abs__(self: Self) -> Self:
        ...


class SupportsAdd(Protocol):
    def __add__(self: Self, other: Self) -> Self:
        ...


class SupportsMul(Protocol):
    def __mul__(self: Self, other: Self) -> Self:
        ...


class SupportsNat(
    SupportsRichComparison,
    SupportsAbs,
    SupportsAdd,
    SupportsMul,
    Protocol,
):
    def __int__(self: Self) -> int:
        ...


class SupportsNeg(Protocol):
    def __neg__(self: Self) -> Self:
        ...


class SupportsSub(Protocol):
    def __sub__(self: Self, other: Self) -> Self:
        ...


class SupportsInt(
    SupportsNat,
    SupportsNeg,
    SupportsSub,
    Protocol,
):
    ...


class SupportsDiv(Protocol):
    def __truediv__(self: Self, other: Self) -> Self:
        ...


class SupportsPow2(Protocol):
    def __pow__(self: Self, other: int) -> Self:
        ...


class SupportsRat(
    SupportsInt,
    SupportsDiv,
    SupportsPow2,
    Protocol,
):
    def __float__(self: Self) -> float:
        ...
