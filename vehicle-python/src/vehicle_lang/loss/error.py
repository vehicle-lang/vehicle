from dataclasses import dataclass

from ..error import VehicleError as VehicleError
from ..error import VehicleInternalError


@dataclass(frozen=True)
class VehicleBuiltinUnsupported(VehicleError):
    builtin_name: str


@dataclass(frozen=True)
class VehiclePropertyNotFound(VehicleInternalError):
    property_name: str


@dataclass(frozen=True)
class VehiclePropertyNotCallable(VehicleInternalError):
    property_name: str
