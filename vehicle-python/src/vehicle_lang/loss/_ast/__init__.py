from pathlib import Path
from typing import Iterable

from ... import session as session
from ...typing import DeclarationName, DifferentiableLogic, Target
from ..error import VehicleError as VehicleError
from . import _nodes


def load(
    path: str | Path,
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = DifferentiableLogic.Vehicle,
) -> _nodes.Program:
    exc, out, err, log = session.check_output(
        [
            "--json",
            "compile",
            "loss",
            "--logic",
            target._vehicle_option_name,
            f"--specification={path}",
            *[f"--declaration={declaration_name}" for declaration_name in declarations],
        ]
    )
    if exc != 0:
        msg: str = err or out or log or "unknown error"
        raise VehicleError(msg)
    if out is None:
        raise VehicleError("no output")
    return _nodes.Program.from_json(out)


__all__ = [
    "_nodes",
    "load",
]
