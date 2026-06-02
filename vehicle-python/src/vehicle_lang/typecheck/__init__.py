from enum import Enum
from pathlib import Path
from typing import List, Optional

from .. import session
from ..error import VehicleUserError


class SecondaryTypeSystem(Enum):
    """
    The type systems supported by Vehicle.
    """

    Polarity = 1
    Linearity = 2
    Decidability = 3

    @property
    def _vehicle_option_name(self) -> str:
        return {
            SecondaryTypeSystem.Polarity: "Polarity",
            SecondaryTypeSystem.Linearity: "Linearity",
            SecondaryTypeSystem.Decidability: "Decidability",
        }[self]


def typecheck(
    specification: str | Path,
) -> Optional[VehicleUserError]:
    """
    Type-check a .vcl specification file.

    :param specification: The path to the Vehicle specification file to verify.
    :returns: None if type-checking succeeded, or a `VehicleUserError` if type-checking failed with a user error.
    :raises VehicleInternalError: If the Vehicle command fails to execute.
    """
    args = [
        "--json",
        "typecheck",
        "--specification",
        str(specification),
    ]

    # Call Vehicle
    try:
        session.execute_command(args)
        return None
    except VehicleUserError as err:
        return err


def typecheck_with_typesystem(
    specification: str | Path, typeSystem: SecondaryTypeSystem
) -> str:
    """
    Type-check a .vcl specification file.

    :param specification: The path to the Vehicle specification file to verify.
    :param typeSystem: The secondary type system that should be used.
    :returns: a JSON string containing Vehicle's output.
    :raises VehicleInternalError: If the Vehicle command fails to execute.
    """
    args = [
        "--json",
        "typecheck",
        "--specification",
        str(specification),
        "--type-system",
        typeSystem._vehicle_option_name,
    ]

    # Call Vehicle
    out = session.execute_command(args)
    if not out:
        return ""
    return out


__all__: List[str] = [
    "SecondaryTypeSystem",
    "typecheck",
    "typecheck_with_typesystem",
]
