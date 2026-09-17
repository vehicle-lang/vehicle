"""PyTorch-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterable, Mapping, MutableMapping

from .._deps import require_optional_dependency
from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_loss_specification
from ._pytorch._translation import PyTorchTranslation
from ._pytorch.samplers import DefaultPyTorchSampler, PyTorchSampler

if TYPE_CHECKING:
    import torch
else:  # pragma: no cover - exercised implicitly
    torch = require_optional_dependency(
        "torch",
        extra="pytorch",
        feature="The PyTorch loss backend",
    )

__all__ = [
    "load_specification",
    "PyTorchSampler",
    "DefaultPyTorchSampler",
]


def _validate_domain(
    lower_bound: torch.Tensor,
    upper_bound: torch.Tensor,
) -> None:
    if not torch.all(lower_bound <= upper_bound).item():
        raise ValueError("Empty sampling domain: lower bound exceeds upper bound.")


def load_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Load a loss function compiled for PyTorch."""

    return load_loss_specification(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=PyTorchTranslation,
        default_sampler_factory=DefaultPyTorchSampler,
        domain_validator=_validate_domain,
    )
