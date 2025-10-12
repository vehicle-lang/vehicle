from pathlib import Path
from typing import Union

from .. import session
from ..error import VehicleError


def list(specification: Union[str, Path]) -> str:
    """
    List all networks, datasets, parameters, and properties in the specification.

    :param specification: The path to the Vehicle specification file to list entities for.
    :return: list of entities as JSON.
    """
    args = ["list", "--specification", str(specification), "--json"]

    # Call Vehicle
    exc, out, err, _ = session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(f"{err}")
    elif not out:
        return ""

    return out
