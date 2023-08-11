from dataclasses import dataclass

from ..ast import AST
from ..error import VehicleError, VehicleInternalError


@dataclass(frozen=True)
class VehicleBuiltinUnsupported(VehicleError):
    builtin_name: str


@dataclass(frozen=True)
class VehiclePropertyNotFound(VehicleInternalError):
    property_name: str


@dataclass(frozen=True)
class VehicleOptimiseTypeError(VehicleInternalError):
    ast: AST
