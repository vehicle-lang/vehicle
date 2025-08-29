from pathlib import Path
from typing import Union

from .. import session
from ..error import VehicleError
from ..typing import TypeSystem


def check(
    specification: Union[str, Path], typeSystem: TypeSystem = TypeSystem.Standard
) -> str:
    """
    Type-check a .vcl specification file.

    :param specification: The path to the Vehicle specification file to verify.
    :param typeSystem: The typing system that should be used.
    """
    args = [
        "check",
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
