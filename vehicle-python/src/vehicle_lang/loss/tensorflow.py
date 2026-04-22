"""TensorFlow-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping

from ..typing import DeclarationName, DifferentiableLogic, Target
from ._common import load_loss_specification
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
    logic: Target = DifferentiableLogic.DL2,
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
) -> tuple[dict[str, Any], bool]:
    """Load a loss function compiled for TensorFlow.

    Returns:
        ``(declarations, minimise)``. ``minimise`` is ``True`` for
        loss-oriented logics (``DL2``, ``Vehicle``) and ``False`` for
        robustness-oriented logics (``STL``). See
        :func:`vehicle_lang.loss.pytorch.load_specification` for details.
    """

    return load_loss_specification(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=TensorFlowTranslation,
        default_sampler_factory=DefaultTensorFlowSampler,
    )
