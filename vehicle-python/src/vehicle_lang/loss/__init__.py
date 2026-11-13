"""Backend-specific helpers for Vehicle loss compilation."""

from pathlib import Path
from typing import TYPE_CHECKING, Iterable

from .. import session as session
from .._ast import _nodes
from ..error import VehicleInternalError
from ..typing import (
    DeclarationName,
    DifferentiableLogic,
    LossMode,
    VehicleDifferentiableLogic,
)

__all__ = ["load_ast", "tensorflow", "pytorch"]

if TYPE_CHECKING:  # pragma: no cover - import-time only typing aid
    from . import pytorch as pytorch
    from . import tensorflow as tensorflow


def load_ast(
    path: str | Path,
    *,
    mode: LossMode = LossMode.Training,
    declarations: Iterable[DeclarationName] = (),
    target: DifferentiableLogic = VehicleDifferentiableLogic(),
) -> _nodes.Program:
    args = [
        "--json",
        "compile",
        "loss",
        "--lossMode",
        mode._vehicle_option_name(),
        "--logic",
        target._vehicle_option_name(),
        f"--specification={path}",
        *[f"--declaration={declaration_name}" for declaration_name in declarations],
    ]
    out = session.execute_command(args)
    if out is None:
        raise VehicleInternalError("no output")
    return _nodes.Program.from_json(out)
