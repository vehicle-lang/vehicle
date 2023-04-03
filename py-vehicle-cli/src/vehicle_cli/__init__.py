import sys
from typing import List, Optional

from vehicle_cli._binding import hs_defaultMain as _hs_defaultMain
from vehicle_cli._binding import hs_rts_exit as _hs_rts_exit
from vehicle_cli._binding import hs_rts_init as _hs_rts_init


def cli(args: Optional[List[str]] = None):
    _hs_rts_init(["vehicle"])
    _hs_defaultMain(args or [])
    _hs_rts_exit()


def main():
    cli(sys.argv[1:])
