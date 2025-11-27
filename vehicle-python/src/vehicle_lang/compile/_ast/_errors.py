from dataclasses import dataclass

from ..error import VehicleInternalError
from ._nodes import AST


@dataclass(frozen=True)
class VehicleOptimiseTypeError(VehicleInternalError):
    ast: AST
