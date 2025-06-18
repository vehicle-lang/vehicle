from pathlib import Path
from typing import Union

from .. import session
from ..error import VehicleError


def validate(cache: Union[str, Path]) -> str:
    """
    Validate a verification result to check whether it still holds.

    :param cache: The path to the proof cache used by Vehicle.
    """
    args = ["validate", "--cache", str(cache), "--json"]

    # Call Vehicle
    exc, out, err, _ = session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(f"{err}")
    elif not out:
        return ""

    return out
