import json
from typing import Optional, Sequence

from .._ast._decode import DecodeError, decode
from ..error import VehicleInternalError, VehicleUserError
from ._session import Session


def check_call(args: Sequence[str]) -> int:
    """
    Execute a Vehicle command and return its exit code.

    :param args: The command-line arguments to pass to Vehicle.
    :return: The exit code of the Vehicle command.
    """
    return Session().__enter__().check_call(args)


def check_output(
    args: Sequence[str],
) -> tuple[int, Optional[str], Optional[str], Optional[str]]:
    """
    Execute a Vehicle command and capture its output.

    Uses PTY-based output capture to handle C-level stdout from the Haskell RTS.

    :param args: The command-line arguments to pass to Vehicle.
    :return: A tuple of (exit_code, stdout, stderr, log_file_content).
    """
    return Session().__enter__().check_output_pty(args)


def execute_command(
    args: Sequence[str],
) -> Optional[str]:
    """
    Execute a Vehicle command and return its output.

    :param args: The command-line arguments to pass to Vehicle.
    :return: The output of the Vehicle command, or None if it failed.
    :raises VehicleInternalError: If the Vehicle command fails to execute.
    :raises VehicleUserError: If the Vehicle command executes but returns a non-zero exit code, indicating a user error in the specification.
    """
    exit_code, out, err, _ = check_output(args)
    if exit_code != 0:
        if err is None:
            raise VehicleInternalError("Vehicle command failed with no error message")

        try:
            raise decode(VehicleUserError, json.loads(err))
        except (json.JSONDecodeError, DecodeError):
            raise VehicleInternalError(err)

    return out


def close() -> None:
    """
    Close the Vehicle session and clean up the Haskell RTS.
    """
    Session().close()


def open(rts_args: Optional[Sequence[str]] = None) -> None:
    """
    Open a Vehicle session and initialize the Haskell RTS.

    :param rts_args: Optional runtime system arguments to pass to the Haskell RTS.
    """
    Session().open(rts_args)
