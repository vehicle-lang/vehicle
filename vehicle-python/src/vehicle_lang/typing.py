from enum import Enum
from typing import Any, Callable, Dict, Iterator

from typing_extensions import Protocol, TypeAlias, TypeVar

_T = TypeVar("_T")

DeclarationName: TypeAlias = str
"""
A name of a top-level declaration in a Vehicle specification (.vcl) file.
"""

QuantifiedVariableName: TypeAlias = str
"""
A name of a quantified variable in a Vehicle specification (.vcl) file.
"""

# Sampler: TypeAlias = Callable[[Callable[[_T], _Bool], Dict[str, Any]], Iterator[_T]]
Sampler: TypeAlias = Callable[[Dict[str, Any]], Iterator[_T]]

AnySamplers: TypeAlias = Dict[str, Sampler[Any]]


class Target(Protocol):
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
    An enumeration of the neural network verifiers supported by Vehicle.
    """

    Marabou = 1
    """
    The Marabou verifier (https://github.com/NeuralNetworkVerification/Marabou) as
    described in:

    Katz, Guy, et al. "The marabou framework for verification and analysis of deep neural networks."
    Computer Aided Verification: 31st International Conference, CAV 2019, New York City, NY, USA,
    July 15-18, 2019, Proceedings, Part I 31. Springer International Publishing, 2019.
    """

    @property
    def _vehicle_option_name(self) -> str:
        return {
            Verifier.Marabou: "Marabou",
        }[self]
