from dataclasses import dataclass


@dataclass(frozen=True)
class VehicleError(Exception):
    pass


@dataclass(frozen=True)
class SessionClosed(VehicleError):
    pass


@dataclass(frozen=True)
class SessionUsed(VehicleError):
    pass


@dataclass(frozen=True)
class BuiltinUnsupported(VehicleError):
    builtin_name: str


@dataclass(frozen=True)
class InternalError(VehicleError):
    pass


@dataclass(frozen=True)
class PropertyNameNotFound(InternalError):
    property_name: str
