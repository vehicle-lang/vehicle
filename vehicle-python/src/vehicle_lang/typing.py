from abc import ABCMeta, abstractmethod
from enum import Enum
from typing import Any, Callable, Dict, Generic, Iterator, Sequence

from typing_extensions import Protocol, TypeAlias, TypeVar

_T = TypeVar("_T")
_R = TypeVar("_R")
_Variable = TypeVar("_Variable")
_VariableValue = TypeVar("_VariableValue")

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

Context: TypeAlias = Dict[QuantifiedVariableName, _T]
"""
The variable context, a mapping from variable names to values.
"""

################################################################################
## Variable domains
################################################################################


class AbstractVariableDomain(Generic[_VariableValue], metaclass=ABCMeta):
    """
    An abstract interface for the domain of a quantified variable,
    i.e. the set of values the variable is allowed to take.
    Parameterised by the type o.
    """

    @abstractmethod
    def dimensions(self) -> Sequence[int]:
        """
        Return the dimensions of the domain.
        """
        ...

    @abstractmethod
    def random_value(self) -> _VariableValue:
        """
        Generate a random value that is guaranteed to lie within the domain.
        """
        ...

    @abstractmethod
    def clip(self, value: _VariableValue) -> _VariableValue:
        """
        Clips a value so that is guaranteed to lie within in the domain.
        """
        ...


DomainFunction: TypeAlias = Callable[
    [
        Context[_T],
    ],
    AbstractVariableDomain[_VariableValue],
]
"""
A function from the current context to the domain of a quantified variable.
"""

AnyDomain: TypeAlias = DomainFunction[Any, Any]
"""
An optimiser that promises to work for any type.
"""

AnyDomains: TypeAlias = Dict[QuantifiedVariableName, AnyDomain]
"""
A mapping from quantified variable names to domains.
"""

################################################################################
## Quantifier optimisers
################################################################################

Optimiser: TypeAlias = Callable[
    [
        _Variable,
        AbstractVariableDomain[_VariableValue],
        bool,
        Context[_T],
        Callable[[_R, _R], _R],
        Callable[[_T], _R],
    ],
    _R,
]
"""
A function that tries to optimise a variable.
"""

AnyOptimiser: TypeAlias = Optimiser[Any, Any, Any, Any]
"""
An optimiser that promises to work for any type.
"""

AnyOptimisers: TypeAlias = Dict[QuantifiedVariableName, AnyOptimiser]
"""
A mapping from quantified variable names to optimisers.
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
