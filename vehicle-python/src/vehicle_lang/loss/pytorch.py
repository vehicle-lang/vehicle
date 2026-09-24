"""PyTorch-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping

from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_loss_specification
from ._pytorch._translation import PyTorchTranslation
from ._pytorch.samplers import DefaultPyTorchSampler, PyTorchSampler

__all__ = [
    "load_specification",
    "PyTorchSampler",
    "DefaultPyTorchSampler",
]


def load_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
    device: Any = None,
) -> dict[str, Any]:
    """Load a loss function compiled for PyTorch.

    `device` is where the compiled code creates its literals and constants; pass the device
    the network and the resources live on. Left unset they are created on the CPU.
    """

    return load_loss_specification(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=lambda: PyTorchTranslation(device=device),
        default_sampler_factory=DefaultPyTorchSampler,
    )
