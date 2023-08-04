from dataclasses import dataclass


@dataclass(frozen=True)
class VehicleError(Exception):
    pass


@dataclass(frozen=True)
class VehicleSessionClosed(VehicleError):
    pass


@dataclass(frozen=True)
class VehicleSessionUsed(VehicleError):
    pass


@dataclass(frozen=True)
class VehicleBuiltinUnsupported(VehicleError):
    builtin_name: str


@dataclass(frozen=True)
class VehicleInternalError(VehicleError):
    pass


@dataclass(frozen=True)
class VehiclePropertyNotFound(VehicleInternalError):
    property_name: str
