from dataclasses import dataclass
from ..error import VehicleInternalError
from .nodes import AST

@dataclass(frozen=True)
class VehicleOptimiseTypeError(VehicleInternalError):
    ast: AST
