from typing import List

from vehicle_compiler._binding import hs_defaultMain as _hs_defaultMain
from vehicle_compiler._binding import hs_rts_exit as _hs_rts_exit
from vehicle_compiler._binding import hs_rts_init as _hs_rts_init


def main(args: List[str]):
    _hs_rts_init(["vehicle"])
    _hs_defaultMain(args)
    _hs_rts_exit()
