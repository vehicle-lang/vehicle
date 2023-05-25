import contextlib
from typing import Iterator, List, Optional, Tuple

from vehicle_lang._binding import (
    _unsafe_vehicle_free,
    _unsafe_vehicle_init,
    _unsafe_vehicle_main,
)
from vehicle_lang._tempfiles import _tempfiles


def vehicle(
    args: Optional[List[str]] = None,
) -> Tuple[int, Optional[str], Optional[str], Optional[str]]:
    """Calls the Vehicle main function with the given arguments and returns the exit code, output, error, and logs."""
    with _tempfiles("out", "err", "log", prefix="vehicle") as (out, err, log):
        exc = _vehicle_cli(
            [
                f"--redirect-stdout={out}",
                f"--redirect-stderr={err}",
                f"--redirect-logs={log}",
                *(args or []),
            ]
        )
        return (exc, out.read_text(), err.read_text(), log.read_text())


def _vehicle_cli(args: Optional[List[str]] = None) -> int:
    """Calls the Vehicle main function with the given arguments and returns the exit code."""
    with _vehicle_rts():
        return _unsafe_vehicle_main(args or [])


@contextlib.contextmanager
def _vehicle_rts(args: Optional[List[str]] = None) -> Iterator[None]:
    try:
        _unsafe_vehicle_init(["vehicle", *(args or [])])
        yield None
    finally:
        _unsafe_vehicle_free()
