from abc import ABCMeta, abstractmethod
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


class DifferentiableLogic(metaclass=ABCMeta):

    @abstractmethod
    def _vehicle_option_name(self) -> str: ...


class VehicleDifferentiableLogic(DifferentiableLogic):

    def _vehicle_option_name(self) -> str:
        return "VehicleLoss"


class DL2DifferentiableLogic(DifferentiableLogic):

    def _vehicle_option_name(self) -> str:
        return "DL2Loss"


class CustomDifferentiableLogic(DifferentiableLogic):

    def __init__(self, name: str) -> None:
        self._name = name

    def _vehicle_option_name(self) -> str:
        return self._name


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


class LossMode(Enum):
    """
    The use cases for which loss functions can be generated.
    """

    Training = 1
    Search = 2

    def _vehicle_option_name(self) -> str:
        return {
            LossMode.Training: "Training",
            LossMode.Search: "Search",
        }[self]


class QueryFormat(Enum):
    """
    The query formats supported by Vehicle.
    """

    VNNLib = 1
    Marabou = 2

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

    def _vehicle_option_name(self) -> str:
        return {ITP.Agda: "Agda", ITP.Rocq: "Rocq"}[self]


class ExportTarget(Enum):
    """
    The target to export to. Only Agda is currently supported.
    """

    Agda = 1
    Rocq = 2

    def _vehicle_option_name(self) -> str:
        return {ExportTarget.Agda: "Agda", ExportTarget.Rocq: "Rocq"}[self]
