from typing import List
from vehicle.core._binding import (
    hs_defaultMain as _hs_defaultMain,
    hs_rts_init as _hs_rts_init,
    hs_rts_exit as _hs_rts_exit,
)


def main(args: List[str]):
    _hs_rts_init(["vehicle"])
    _hs_defaultMain(args)
    _hs_rts_exit()
