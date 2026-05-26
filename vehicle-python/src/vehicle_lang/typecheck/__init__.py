from enum import Enum
from pathlib import Path
from typing import List, Optional

from .. import session


class TypeSystem(Enum):
    """
    The type systems supported by Vehicle.
    """

    Standard = 1
    Polarity = 2
    Linearity = 3
    Decidability = 4

    @property
    def _vehicle_option_name(self) -> Optional[str]:
        return {
            TypeSystem.Standard: None,
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
        "--json",
        "typecheck",
        "--specification",
        str(specification),
    ]

    if typeSystem._vehicle_option_name is not None:
        args += ["--type-system", typeSystem._vehicle_option_name]

    # Call Vehicle
    out = session.execute_command(args)
    if not out:
        return ""

    return out


__all__: List[str] = ["TypeSystem", "typecheck"]
