from enum import Enum
from pathlib import Path
from typing import List

from .. import session
from ..error import VehicleError


class TypeSystem(Enum):
    """
    The type systems supported by Vehicle.
    """

    Standard = 1
    Polarity = 2
    Linearity = 3
    Decidability = 4

    @property
    def _vehicle_option_name(self) -> str:
        return {
            TypeSystem.Standard: "Standard",
            TypeSystem.Polarity: "Polarity",
            TypeSystem.Linearity: "Linearity",
            TypeSystem.Decidability: "Decidability",
        }[self]


def typecheck(
    specification: str | Path, *, typeSystem: TypeSystem = TypeSystem.Standard
) -> str:
    """
    Type-check a .vcl specification file.

    :param specification: The path to the Vehicle specification file to verify.
    :param typeSystem: The type system that should be used.
    """
    args = [
        "typecheck",
        "--specification",
        str(specification),
        "--typeSystem",
        typeSystem._vehicle_option_name,
        "--json",
    ]

    # Call Vehicle
    exc, out, err, _ = session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(f"{err}")
    elif not out:
        return ""

    return out


__all__: List[str] = ["TypeSystem", "typecheck"]
