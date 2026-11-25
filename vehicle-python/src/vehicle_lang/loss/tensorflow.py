"""TensorFlow-specific loss helpers."""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterable, Mapping

from .._deps import require_optional_dependency
from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import BoundVarData, load_search_loss, load_training_loss
from ._search_tree import SearchResults, search_properties
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
    "search",
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


def search(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: Mapping[str, TensorFlowSampler] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: dict[str, Any] | None = None,
    networks: dict[DeclarationName, Any] = {},
    datasets: dict[DeclarationName, Any] = {},
    parameters: dict[DeclarationName, Any] = {},
    num_searches: int = 1,
) -> dict[str, SearchResults]:
    """Gradient-based search for properties in a specification."""

    search_data = load_search_loss(
        path,
        logic=logic,
        declarations=declarations,
        declaration_context=declaration_context,
        networks=networks,
        datasets=datasets,
        parameters=parameters,
        translation_factory=TensorFlowTranslation,
    )

    search_samplers: Mapping[str, TensorFlowSampler]
    if samplers is None:
        search_samplers = defaultdict(_default_search_sampler)
    else:
        search_samplers = samplers

    return search_properties(
        search_data,
        samplers=search_samplers,
        initial_point=_initial_point,
        num_searches=num_searches,
    )


def _default_search_sampler() -> TensorFlowSampler:
    # By default, we only optimise each bound variable for one trajectory with 10 steps
    return DefaultTensorFlowSampler(num_samples=1, num_steps=10)


def _initial_point(bound_var: BoundVarData) -> tf.Tensor:
    """Generates a uniformly random starting point within the bound variable's bounds."""
    lower_bound = tf.cast(bound_var.lower_bound, tf.float32)
    upper_bound = tf.cast(bound_var.upper_bound, tf.float32)
    range_size = upper_bound - lower_bound
    return (
        lower_bound
        + tf.random.uniform(shape=bound_var.dims, dtype=tf.float32) * range_size
    )
