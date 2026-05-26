"""Backend-specific helpers for Vehicle loss compilation."""

from typing import TYPE_CHECKING
from pathlib import Path
from typing import Iterable

from .. import session as session
from ..typing import DeclarationName, DifferentiableLogic, Target
from ..error import VehicleInternalError
from .._ast import _nodes

__all__ = ["tensorflow", "pytorch"]

if TYPE_CHECKING:  # pragma: no cover - import-time only typing aid
    from . import pytorch as pytorch
    from . import tensorflow as tensorflow

def load_ast(
    path: str | Path,
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = DifferentiableLogic.Vehicle,
) -> _nodes.Program:
    args = [
            "--json",
            "compile",
            "loss",
            "--logic",
            target._vehicle_option_name,
            f"--specification={path}",
            *[f"--declaration={declaration_name}" for declaration_name in declarations],
    ]
    out = session.execute_command(args)
    if out is None:
        raise VehicleInternalError("no output")
    return _nodes.Program.from_json(out)
