from typing import Dict


class VehicleError(Exception):
    pass


class VehicleSessionClosed(VehicleError):
    pass


class VehicleSessionUsed(VehicleError):
    pass


class VehicleJsonMalformed(VehicleError):
    pass


class VehicleDeveloperError(VehicleError):
    pass
