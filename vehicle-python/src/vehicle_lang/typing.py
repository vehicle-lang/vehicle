from enum import Enum
from typing import Any, Callable, Dict, Iterator

from typing_extensions import Protocol, TypeAlias, TypeVar

_T = TypeVar("_T")
_R = TypeVar("_R")


DeclarationName: TypeAlias = str
"""
A name of a top-level declaration in a Vehicle specification file.
"""

QuantifiedVariableName: TypeAlias = str
"""
A name of a quantified variable in a Vehicle specification file.
"""

Optimiser: TypeAlias = Callable[
    [
        bool,
        Dict[QuantifiedVariableName, _T],
        Callable[[_R, _R], _R],
        Callable[[_T], _R],
    ],
    _R,
]
"""
TODO: add description
"""

AnyOptimiser: TypeAlias = Optimiser[Any, Any]
"""
An optimiser that promises to work for any type.
"""

AnyOptimisers: TypeAlias = Dict[str, AnyOptimiser]
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
