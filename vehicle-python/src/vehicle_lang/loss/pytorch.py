"""PyTorch-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from dataclasses import dataclass
from typing import Any, Iterable, Mapping, MutableMapping, Sequence

from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_training_loss, load_search_loss
from ._pytorch._translation import PyTorchTranslation
from ._pytorch.samplers import DefaultPyTorchSampler, PyTorchSampler, Sample

import torch

__all__ = [
    "load_specification",
    "PyTorchSampler",
    "DefaultPyTorchSampler",
]

@dataclass
class SearchResult:
    property: str
    witnesses: Sequence[Sample]
    adversarial_examples: Sequence[Sample]


def load_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
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
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
    networks: dict[DeclarationName, Any] = {},
    datasets: dict[DeclarationName, Any] = {},
    parameters: dict[DeclarationName, Any] = {},
    num_samples: int = 10,
    num_steps: int = 5,
    seed: int | None = None
) -> Sequence[SearchResult]:
    """
    Generates samples for each property in a specification.

    If the property contains only universal quantifiers, the samples generated
    are adversarial examples to the property.

    If the property contains only existential quantifiers, the samples generated
    are witnesses to the property.
    """

    search_spec = load_search_loss(
        path,
        logic=logic,
        declarations=declarations,
        declaration_context=declaration_context,
        networks=networks,
        datasets=datasets,
        parameters=parameters,
        translation_factory=PyTorchTranslation
    )

    declarations = search_spec.declarations
    property_data = search_spec.property_data
    search_bounds = search_spec.search_bounds

    sampler = DefaultPyTorchSampler(num_samples=num_samples, num_steps=num_steps, seed=seed)

    search_results = []
    for property, contains_forall in property_data.items():
        samples = sampler.get_samples(bound_vars=search_bounds[property], loss_fn=declarations[property])

        if contains_forall:
            result = SearchResult(property=property, witnesses=[], adversarial_examples=samples)
        else:
            result = SearchResult(property=property, witnesses=samples, adversarial_examples=[])

        search_results.append(result)

    return search_results