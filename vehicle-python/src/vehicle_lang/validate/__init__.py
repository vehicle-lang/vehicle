from pathlib import Path

from .. import session


def validate(cache: str | Path) -> str:
    """
    Validate a verification result to check whether it still holds.

    :param cache: The path to the proof cache used by Vehicle.
    """
    args = ["validate", "--cache", str(cache), "--json"]

    # Call Vehicle
    out = session.execute_command(args)
    if not out:
        return ""

    return out
