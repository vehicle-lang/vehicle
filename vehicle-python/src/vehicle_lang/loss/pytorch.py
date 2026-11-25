"""PyTorch-specific loss helpers."""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterable, Mapping

from .._deps import require_optional_dependency
from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import BoundVarData, load_search_loss, load_training_loss
from ._pytorch._translation import PyTorchTranslation
from ._pytorch.samplers import DefaultPyTorchSampler, PyTorchSampler
from ._search_tree import SearchResults, search_properties

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
    "search",
    "PyTorchSampler",
    "DefaultPyTorchSampler",
]


def load_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Load a loss function compiled for PyTorch."""

    return load_training_loss(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=PyTorchTranslation,
        default_sampler_factory=DefaultPyTorchSampler,
    )


def search(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: Mapping[str, Any] | None = None,
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
        translation_factory=PyTorchTranslation,
    )

    search_samplers: Mapping[str, PyTorchSampler]
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


def _default_search_sampler() -> PyTorchSampler:
    # By default, we only optimise each bound variable for one trajectory with 10 steps
    return DefaultPyTorchSampler(num_samples=1, num_steps=10)


def _initial_point(bound_var: BoundVarData) -> torch.Tensor:
    """Generates a uniformly random starting point within the bound variable's bounds."""
    lower_bound: torch.Tensor = bound_var.lower_bound
    upper_bound: torch.Tensor = bound_var.upper_bound
    range_size = upper_bound - lower_bound
    return (
        lower_bound + torch.rand(bound_var.dims, dtype=lower_bound.dtype) * range_size
    )
