import contextlib
from typing import Iterator, List, Optional

from vehicle_lang._binding import _hs_rts_exit, _hs_rts_init, _unsafeVehicleMain


def unsafeVehicleMain(args: Optional[List[str]] = None) -> int:
    """Calls the Vehicle main function with the given arguments and returns the exit code."""
    with _hs_rts(["vehicle"]):
        return _unsafeVehicleMain(args or [])


@contextlib.contextmanager
def _hs_rts(args: List[str]) -> Iterator[None]:
    try:
        _hs_rts_init(args)
        yield None
    finally:
        _hs_rts_exit()
