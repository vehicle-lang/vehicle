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
    The built-in differentiable logics supported by Vehicle.

    For custom logics defined in a Vehicle specification, use
    :class:`CustomLogic` instead.
    """

    Vehicle = 1
    DL2 = 2
    STL = 3
    # Godel = 4
    # Lukasiewicz = 5
    # Product = 6
    # Yager = 7

    @property
    def _vehicle_option_name(self) -> str:
        return {
            DifferentiableLogic.Vehicle: "VehicleLoss",
            DifferentiableLogic.DL2: "DL2Loss",
            DifferentiableLogic.STL: "STLLoss",
            # Currently unsupported options
            # DifferentiableLogic.Godel: "GodelLoss",
            # DifferentiableLogic.Lukasiewicz: "LukasiewiczLoss",
            # DifferentiableLogic.Product: "ProductLoss",
            # DifferentiableLogic.Yager: "YagerLoss",
        }[self]


class CustomLogic:
    """
    A user-defined differentiable logic.

    Use this to reference a custom ``DifferentiableTensorLogic`` definition
    from your Vehicle specification by name::

        logic = CustomLogic("MyCustomLoss")
        specs = load_specification("spec.vcl", logic=logic)
    """

    def __init__(self, name: str) -> None:
        self._name = name

    @property
    def _vehicle_option_name(self) -> str:
        return self._name

    def __repr__(self) -> str:
        return f"CustomLogic({self._name!r})"


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
