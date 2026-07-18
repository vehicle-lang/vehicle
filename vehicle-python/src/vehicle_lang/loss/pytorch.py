"""PyTorch-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping

from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_training_specification, load_search_specification
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
) -> dict[str, Any]:
    """Load a loss function compiled for PyTorch."""

    training_spec = load_training_specification(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=PyTorchTranslation,
        default_sampler_factory=DefaultPyTorchSampler,
    )
    return training_spec.declarations

def search(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
):
    # Probably will call gradient based attacks here?
    return load_search_specification(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=PyTorchTranslation,
        default_sampler_factory=DefaultPyTorchSampler,
    )