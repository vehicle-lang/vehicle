import sys
from typing import List, Optional, Tuple

from vehicle_lang._internal import unsafeVehicleMain
from vehicle_lang._tempfiles import tempfiles
from vehicle_lang._version import VERSION as VERSION


def vehicleSync(
    args: Optional[List[str]] = None,
) -> Tuple[int, Optional[str], Optional[str], Optional[str]]:
    with tempfiles("out", "err", "log", prefix="vehicle") as (out, err, log):
        exc = unsafeVehicleMain(
            [
                f"--redirect-stdout={out}",
                f"--redirect-stderr={err}",
                f"--redirect-logs={log}",
                *(args or []),
            ]
        )
        return (exc, out.read_text(), err.read_text(), log.read_text())


def main():
    exit(unsafeVehicleMain(sys.argv[1:]))
