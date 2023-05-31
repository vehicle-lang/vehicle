class VehicleError(Exception):
    pass


class VehicleSessionClosed(VehicleError):
    pass


class VehicleSessionUsed(VehicleError):
    pass
