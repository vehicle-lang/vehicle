from typing import Optional, Sequence, Tuple, Union, overload

from typing_extensions import Literal

from ._version import VERSION as VERSION
from .session import Session


@overload
def vehicle(args: Sequence[str], *, capture_output: Literal[False]) -> int:
    ...


@overload
def vehicle(
    args: Sequence[str], *, capture_output: Literal[True] = True
) -> Tuple[int, Optional[str], Optional[str], Optional[str]]:
    ...


def vehicle(
    args: Sequence[str], *, capture_output: bool = True
) -> Union[int, Tuple[int, Optional[str], Optional[str], Optional[str]]]:
    with Session() as vehicle:
        if capture_output:
            return vehicle.check_output(args)
        else:
            return vehicle.check_call(args)
