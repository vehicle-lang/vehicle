from enum import Enum

from typing_extensions import Protocol, TypeAlias

DeclarationName: TypeAlias = str
"""
A name of a top-level declaration in a Vehicle specification file.
"""

QuantifiedVariableName: TypeAlias = str
"""
A name of a quantified variable in a Vehicle specification file.
"""


class Target(Protocol):
    """
    Translation targets from Vehicle to Python.

    Valid values are either `Explicit` or any member of `DifferentiableLogic`.
    """

    @property
    def _vehicle_option_name(self) -> str: ...


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
    # Godel = 3
    # Lukasiewicz = 4
    # Product = 5
    # Yager = 6

    @property
    def _vehicle_option_name(self) -> str:
        return {
            DifferentiableLogic.Vehicle: "VehicleLoss",
            DifferentiableLogic.DL2: "DL2Loss",
            # Currently unsupported options
            # DifferentiableLogic.Godel: "GodelLoss",
            # DifferentiableLogic.Lukasiewicz: "LukasiewiczLoss",
            # DifferentiableLogic.Product: "ProductLoss",
            # DifferentiableLogic.Yager: "YagerLoss",
        }[self]


class LossBackend(Enum):
    """
    The backends supported for loss function computation.
    """

    TensorFlow = 1
    PyTorch = 2

    @property
    def _vehicle_option_name(self) -> str:
        return {
            LossBackend.TensorFlow: "TensorFlow",
            LossBackend.PyTorch: "PyTorch",
        }[self]


class QueryFormat(Enum):
    """
    The query formats supported by Vehicle.
    """

    VNNLib = 1
    Marabou = 2

    @property
    def _vehicle_option_name(self) -> str:
        return {
            QueryFormat.VNNLib: "VNNLibQueries",
            QueryFormat.Marabou: "MarabouQueries",
        }[self]


class ITP(Enum):
    """
    The ITPs supported by Vehicle.
    """

    Agda = 1
    Rocq = 2

    @property
    def _vehicle_option_name(self) -> str:
        return {ITP.Agda: "Agda", ITP.Rocq: "Rocq"}[self]


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


class ExportTarget(Enum):
    """
    The target to export to. Only Agda is currently supported.
    """

    Agda = 1
    Rocq = 2

    @property
    def _vehicle_option_name(self) -> str:
        return {ExportTarget.Agda: "Agda", ExportTarget.Rocq: "Rocq"}[self]
