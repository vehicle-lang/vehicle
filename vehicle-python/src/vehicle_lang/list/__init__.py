from pathlib import Path
from typing import Union

from .. import session
from ..error import VehicleError


def list_resources(specification: Union[str, Path]) -> str:
    """
    List all networks, datasets, and parameters in the specification.

    :param specification: The path to the Vehicle specification file to list resources for.
    :return: list of entities as JSON.
    """
    args = ["list", "resources", "--specification", str(specification), "--json"]

    # Call Vehicle
    exc, out, err, _ = session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(f"{err}")
    elif not out:
        return ""

    return out


def list_properties(specification: Union[str, Path]) -> str:
    """
    List all properties in the specification.

    :param specification: The path to the Vehicle specification file to list properties for.
    :return: list of entities as JSON.
    """
    args = ["list", "properties", "--specification", str(specification), "--json"]

    # Call Vehicle
    exc, out, err, _ = session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(f"{err}")
    elif not out:
        return ""

    return out
