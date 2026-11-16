"""Test that the error classes can be raised through a context manager.

On Python 3.11 and later `contextlib` assigns to `__traceback__` when re-raising, which a frozen
dataclass rejects, replacing the error with a `FrozenInstanceError`.
"""

import subprocess
import sys
from contextlib import contextmanager
from typing import Iterator

import pytest
from vehicle_lang.error import VehicleInternalError, VehicleUserError
from vehicle_lang.session.error import VehicleSessionClosed, VehicleSessionUsed

ERRORS = [
    VehicleInternalError("internal"),
    VehicleUserError(None, "problem", "fix"),
    VehicleSessionClosed(),
    VehicleSessionUsed(),
]


@contextmanager
def context() -> Iterator[None]:
    yield


@pytest.mark.parametrize("error", ERRORS)  # type: ignore[untyped-decorator]
def test_error_survives_context_manager(error: Exception) -> None:
    with pytest.raises(type(error)):
        with context():
            raise error


# `Session.check_output` runs the compiler inside `temporary_files`, so an error raised there has to
# survive the manager to reach the caller. Closing the session frees the Haskell RTS for the rest of
# the process, so this runs in a subprocess.
_CLOSED_SESSION = """
from vehicle_lang import session

session.open()
session.close()
try:
    session.check_output(["--version"])
except BaseException as error:
    print(type(error).__qualname__)
"""


def test_session_error_survives_temporary_files() -> None:
    result = subprocess.run(
        [sys.executable, "-c", _CLOSED_SESSION],
        capture_output=True,
        encoding="utf-8",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "VehicleSessionClosed"
