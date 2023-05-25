from contextlib import contextmanager
from typing import Iterator, Optional, Sequence, Tuple

from vehicle_lang._binding import (
    _unsafe_vehicle_free,
    _unsafe_vehicle_init,
    _unsafe_vehicle_main,
)
from vehicle_lang._temporary_files import temporary_files


def vehicle(
    args: Optional[Sequence[str]] = None,
) -> Tuple[int, Optional[str], Optional[str], Optional[str]]:
    """Calls the Vehicle main function with the given arguments and returns the exit code, output, error, and logs."""
    with temporary_files("out", "err", "log", prefix="vehicle") as (out, err, log):
        exc = _vehicle_cli(
            [
                f"--redirect-stdout={out}",
                f"--redirect-stderr={err}",
                f"--redirect-logs={log}",
                *(args or []),
            ]
        )
        return (exc, out.read_text(), err.read_text(), log.read_text())


def _vehicle_cli(args: Optional[Sequence[str]] = None) -> int:
    """Calls the Vehicle main function with the given arguments and returns the exit code."""
    with _vehicle_rts():
        return _unsafe_vehicle_main(args or [])


@contextmanager
def _vehicle_rts(args: Optional[Sequence[str]] = None) -> Iterator[None]:
    try:
        _unsafe_vehicle_init(["vehicle", *(args or [])])
        yield None
    finally:
        _unsafe_vehicle_free()
