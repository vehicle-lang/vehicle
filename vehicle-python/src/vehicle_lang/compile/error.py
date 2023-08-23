from dataclasses import dataclass

from ..ast import AST
from ..error import VehicleError, VehicleInternalError
from ..typing import DeclarationName, QuantifiedVariableName


@dataclass(frozen=True)
class VehicleBuiltinUnsupported(VehicleError):
    builtin_name: str


@dataclass(frozen=True)
class VehiclePropertyNotFound(VehicleInternalError):
    property_name: DeclarationName


@dataclass(frozen=True)
class VehicleOptimiseTypeError(VehicleInternalError):
    ast: AST


@dataclass(frozen=True)
class VehicleDomainNotFound(VehicleInternalError):
    quantified_variable_name: QuantifiedVariableName


@dataclass(frozen=True)
class VehicleOptimiserNotFound(VehicleInternalError):
    quantified_variable_name: QuantifiedVariableName
