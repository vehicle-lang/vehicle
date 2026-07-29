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

    def __str__(self) -> str:
        return f"Vehicle threw an unexpected error: {self.message}"


@dataclass(frozen=True)
class VehicleUserError(VehicleError):
    """
    An error indicating a problem in the Vehicle specification encountered during compilation.
    """

    provenance: Optional[Provenance]
    problem: str
    fix: Optional[str]

    def __str__(self) -> str:
        location = f" at {self.provenance}" if self.provenance else ""
        fix_suggestion = f" Suggested fix: {self.fix}" if self.fix else ""
        return f"Error{location}: {self.problem}.{fix_suggestion}"
