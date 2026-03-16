from typing import Optional, Sequence

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
