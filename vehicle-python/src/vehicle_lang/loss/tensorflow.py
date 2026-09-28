"""TensorFlow-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping

from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_training_loss
from ._tensorflow._translation import TensorFlowTranslation
from ._tensorflow.samplers import DefaultTensorFlowSampler, TensorFlowSampler

__all__ = [
    "load_specification",
    "TensorFlowSampler",
    "DefaultTensorFlowSampler",
]


def load_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: dict[str, TensorFlowSampler] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Load a loss function compiled for TensorFlow."""

    return load_training_loss(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=TensorFlowTranslation,
        default_sampler_factory=DefaultTensorFlowSampler,
    )
