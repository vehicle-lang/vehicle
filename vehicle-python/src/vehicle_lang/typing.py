from abc import ABCMeta, abstractmethod
from enum import Enum
from typing import Any, Callable, Dict, Generic, Iterable, Tuple

from typing_extensions import Protocol, Self, TypeAlias, TypeVar, runtime_checkable

_Loss = TypeVar("_Loss")
_Variable = TypeVar("_Variable")

_T = TypeVar("_T")
_T_co = TypeVar("_T_co", covariant=True)

################################################################################
# Protocols for Vehicle's builtin List and Vector types
################################################################################


@runtime_checkable
class VehicleList(Iterable[_T_co], Protocol[_T_co]):
    pass


@runtime_checkable
class Subscriptable(Protocol[_T_co]):
    def __getitem__(self, index: int) -> _T_co:
        ...


@runtime_checkable
class VehicleVector(Iterable[_T_co], Subscriptable[_T_co], Protocol[_T_co]):
    pass


################################################################################
# Protocols for Vehicle's builtin Nat, Int, and Rat types
################################################################################


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
class VehicleNat(
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
class VehicleInt(
    VehicleNat,
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
class VehicleRat(
    VehicleInt,
    SupportsDiv,
    SupportsPow2,
    Protocol,
):
    def __float__(self: Self) -> float:
        ...


################################################################################
## Names
################################################################################


DeclarationName: TypeAlias = str
"""
A name of a top-level declaration in a Vehicle specification file.
"""

QuantifiedVariableName: TypeAlias = str
"""
A name of a quantified variable in a Vehicle specification file.
"""

Context: TypeAlias = Dict[QuantifiedVariableName, _Loss]
"""
The variable context, a mapping from variable names to values.
"""

AnyContext: TypeAlias = Context[Any]

################################################################################
## Variable domains
################################################################################


class VariableDomain(Generic[_Loss], metaclass=ABCMeta):
    """
    An abstract interface for the domain of a quantified variable, i.e., the
    set of values the variable is allowed to take.
    """

    @property
    @abstractmethod
    def shape(self) -> Tuple[int, ...]:
        """
        Return the dimensions of the domain.
        """
        ...

    @abstractmethod
    def random_value(self) -> VehicleVector[_Loss]:
        """
        Generate a random value that is guaranteed to lie within the domain.
        """
        ...

    @abstractmethod
    def clip(self, value: VehicleVector[_Loss]) -> VehicleVector[_Loss]:
        """
        Clips a value so that is guaranteed to lie within in the domain.
        """
        ...


Domain: TypeAlias = Callable[
    [
        AnyContext,
    ],
    VariableDomain[_Loss],
]
"""
A function from the current context to the domain of a quantified variable.
"""

Domains: TypeAlias = Dict[QuantifiedVariableName, Domain[_Loss]]
"""
A mapping from quantified variable names and contexts to domains.
"""

AnyDomain: TypeAlias = Domain[Any]
"""
An optimiser that promises to work for any type.
"""

AnyDomains: TypeAlias = Dict[QuantifiedVariableName, AnyDomain]

################################################################################
## Quantifier optimisers
################################################################################

Minimise: TypeAlias = bool

Joiner: TypeAlias = Callable[[_Loss, _Loss], _Loss]

Predicate: TypeAlias = Callable[[_T], _Loss]

Optimiser: TypeAlias = Callable[
    [
        _Variable,
        VariableDomain[_Loss],
        Minimise,
        AnyContext,
        Joiner[_Loss],
        Predicate[_T, _Loss],
    ],
    _Loss,
]
"""
A function that tries to optimise a variable.
"""

Optimisers: TypeAlias = Dict[QuantifiedVariableName, Optimiser[_Variable, _Loss, _T]]
"""
A mapping from quantified variable names to optimisers.
"""

AnyOptimiser: TypeAlias = Optimiser[Any, Any, Any]
"""
An optimiser that promises to work for any type.
"""

AnyOptimisers: TypeAlias = Dict[QuantifiedVariableName, AnyOptimiser]
"""
A set of optimisers that promises to work for any type.
"""


class Target(Protocol):
    """
    Translation targets from Vehicle to Python.

    Valid values are either `Explicit` or any member of `DifferentiableLogic`.
    """

    @property
    def _vehicle_option_name(self) -> str:
        ...


class Explicit(Enum):
    """
    The direct translation from Vehicle to Python.
    """

    Explicit = 1

    @property
    def _vehicle_option_name(self) -> str:
        return "Explicit"


class DifferentiableLogic(Enum):
    """
    The differentiable logics supported by Vehicle.
    """

    Vehicle = 1
    DL2 = 2
    Godel = 3
    Lukasiewicz = 4
    Product = 5
    Yager = 6

    @property
    def _vehicle_option_name(self) -> str:
        return {
            DifferentiableLogic.Vehicle: "VehicleLoss",
            DifferentiableLogic.DL2: "DL2Loss",
            DifferentiableLogic.Godel: "GodelLoss",
            DifferentiableLogic.Lukasiewicz: "LukasiewiczLoss",
            DifferentiableLogic.Product: "ProductLoss",
            DifferentiableLogic.Yager: "YagerLoss",
        }[self]


class Verifier(Enum):
    """
    The neural network verifiers supported by Vehicle.
    """

    Marabou = 1
    """
    The `Marabou verifier`_.

    .. _Marabou verifier: https://github.com/NeuralNetworkVerification/Marabou#readme
    """

    @property
    def _vehicle_option_name(self) -> str:
        return {
            Verifier.Marabou: "Marabou",
        }[self]
