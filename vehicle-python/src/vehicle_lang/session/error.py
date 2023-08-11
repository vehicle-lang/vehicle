from dataclasses import dataclass

from ..error import VehicleError


@dataclass(frozen=True)
class VehicleSessionClosed(VehicleError):
    pass


@dataclass(frozen=True)
class VehicleSessionUsed(VehicleError):
    pass
