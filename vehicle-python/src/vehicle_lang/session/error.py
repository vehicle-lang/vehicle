from dataclasses import dataclass

from ..error import VehicleError


@dataclass(eq=False)
class VehicleSessionClosed(VehicleError):
    pass


@dataclass(eq=False)
class VehicleSessionUsed(VehicleError):
    pass
