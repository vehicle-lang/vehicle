import sys
from typing import List, Optional

from vehicle_lang._binding import hs_rts_exit, hs_rts_init, hs_vehicleMain


def cli(args: Optional[List[str]] = None) -> int:
    hs_rts_init(["vehicle"])
    exitCode = hs_vehicleMain(args or [])
    hs_rts_exit()
    return exitCode


def main():
    exitCode = cli(sys.argv[1:])
    exit(exitCode)
