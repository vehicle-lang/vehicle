"""PyTorch-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterable, Mapping, Sequence

from vehicle_lang.loss._search_tree import search_tree

from .._deps import require_optional_dependency
from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_search_loss, load_training_loss
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

SearchResults = Sequence[Sequence[dict[str, Any]]]


def _validate_domain(
    lower_bound: torch.Tensor,
    upper_bound: torch.Tensor,
) -> None:
    if not torch.all(lower_bound <= upper_bound).item():
        raise ValueError(
            "Empty sampling domain: lower bound exceeds upper bound.")


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
        domain_validator=_validate_domain,
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

    declarations = search_data.declarations
    boolean_trees = search_data.boolean_trees
    search_bounds = search_data.search_bounds

    all_search_results: dict[str, SearchResults] = {}
    for property in boolean_trees:
        search_results = []
        for _ in range(num_searches):
            result = search_tree(
                boolean_tree=property,
                declarations=declarations,
                bound_vars=search_bounds,
                samplers=samplers,
            )

            search_results.append(result)

        all_search_results[property.name] = search_results

    return all_search_results
