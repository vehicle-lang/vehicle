from typing_extensions import Protocol, Self, runtime_checkable


@runtime_checkable
class SupportsDunderLT(Protocol):
    def __lt__(self, other: Self) -> bool:
        ...


@runtime_checkable
class SupportsDunderGT(Protocol):
    def __gt__(self, other: Self) -> bool:
        ...


@runtime_checkable
class SupportsDunderLE(Protocol):
    def __le__(self, other: Self) -> bool:
        ...


@runtime_checkable
class SupportsDunderGE(Protocol):
    def __ge__(self, other: Self) -> bool:
        ...


@runtime_checkable
class SupportsRichComparison(
    SupportsDunderLT, SupportsDunderGT, SupportsDunderLE, SupportsDunderGE, Protocol
):
    ...


@runtime_checkable
class SupportsAbs(Protocol):
    def __abs__(self: Self) -> Self:
        ...


@runtime_checkable
class SupportsAdd(Protocol):
    def __add__(self: Self, other: Self) -> Self:
        ...


@runtime_checkable
class SupportsMul(Protocol):
    def __mul__(self: Self, other: Self) -> Self:
        ...


@runtime_checkable
class SupportsNat(
    SupportsRichComparison,
    SupportsAbs,
    SupportsAdd,
    SupportsMul,
    Protocol,
):
    def __int__(self: Self) -> int:
        ...


@runtime_checkable
class SupportsNeg(Protocol):
    def __neg__(self: Self) -> Self:
        ...


@runtime_checkable
class SupportsSub(Protocol):
    def __sub__(self: Self, other: Self) -> Self:
        ...


@runtime_checkable
class SupportsInt(
    SupportsNat,
    SupportsNeg,
    SupportsSub,
    Protocol,
):
    ...


@runtime_checkable
class SupportsDiv(Protocol):
    def __truediv__(self: Self, other: Self) -> Self:
        ...


@runtime_checkable
class SupportsPow2(Protocol):
    def __pow__(self: Self, other: int) -> Self:
        ...


@runtime_checkable
class SupportsRat(
    SupportsInt,
    SupportsDiv,
    SupportsPow2,
    Protocol,
):
    def __float__(self: Self) -> float:
        ...
