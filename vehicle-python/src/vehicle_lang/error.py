from dataclasses import dataclass
from typing import Optional
from ._ast._nodes import Provenance

class VehicleError(Exception):
    pass


@dataclass(frozen=True)
class VehicleInternalError(VehicleError):
    """
    An error indicating a bug in Vehicle itself.
    """
    message: str

    def __str__(self):
        return f"Vehicle threw an unexpected error: {self.message}"


@dataclass(frozen=True)
class VehicleUserError(VehicleError):
    """
    An error indicating a problem in the Vehicle specification encountered during compilation.
    """
    provenance : Optional[Provenance]
    problem : str
    fix : Optional[str]

