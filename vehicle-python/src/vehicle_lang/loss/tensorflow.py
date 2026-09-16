"""TensorFlow-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterable, Mapping, MutableMapping

from .._deps import require_optional_dependency
from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_training_loss
from ._tensorflow._translation import TensorFlowTranslation
from ._tensorflow.samplers import DefaultTensorFlowSampler, TensorFlowSampler

if TYPE_CHECKING:
    import tensorflow as tf
else:  # pragma: no cover - exercised implicitly
    tf = require_optional_dependency(
        "tensorflow",
        extra="tensorflow",
        feature="The TensorFlow loss backend",
    )

__all__ = [
    "load_specification",
    "TensorFlowSampler",
    "DefaultTensorFlowSampler",
]


def _validate_domain(
    lower_bound: tf.Tensor,
    upper_bound: tf.Tensor,
) -> None:
    tf.debugging.assert_less_equal(
        lower_bound,
        upper_bound,
        message="Empty sampling domain: lower bound exceeds upper bound.",
    )


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
        domain_validator=_validate_domain,
    )
