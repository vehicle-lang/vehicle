"""Test that the error classes can be raised through a context manager.

On Python 3.11 and later `contextlib` assigns to `__traceback__` when re-raising, which a frozen
dataclass rejects, replacing the error with a `FrozenInstanceError`.
"""

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
