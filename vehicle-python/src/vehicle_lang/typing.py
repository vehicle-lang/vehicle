from enum import Enum
from typing import Any, Callable, Dict, Iterator

from typing_extensions import Protocol, TypeAlias, TypeVar

_T = TypeVar("_T")


DeclarationName: TypeAlias = str
"""
A name of a top-level declaration in a Vehicle specification file.
"""

QuantifiedVariableName: TypeAlias = str
"""
A name of a quantified variable in a Vehicle specification file.
"""

# Sampler: TypeAlias = Callable[[Callable[[_T], _Bool], Dict[str, Any]], Iterator[_T]]
Sampler: TypeAlias = Callable[[Dict[str, Any]], Iterator[_T]]

AnySamplers: TypeAlias = Dict[str, Sampler[Any]]


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
