"""PyTorch-specific loss helpers."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping, Sequence

import torch

from vehicle_lang.loss._search import search_property

from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from ._common import load_search_loss, load_training_loss
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
    samplers: dict[str, Any] | None = None,
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
    declarations: Iterable[DeclarationName] = (),
    declaration_context: dict[str, Any] | None = None,
    networks: dict[DeclarationName, Any] = {},
    datasets: dict[DeclarationName, Any] = {},
    parameters: dict[DeclarationName, Any] = {},
    num_samples: int = 10,
    num_steps: int = 5,
    seed: int | None = None,
) -> dict[str, Any]:
    """
    Finds counter-examples for properties in a specification using PGD.
    """

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

    sampler = DefaultPyTorchSampler(
        num_samples=num_samples, num_steps=num_steps, seed=seed
    )

    counterexamples = {}
    for property in boolean_trees:
        counterexample = search_property(
            boolean_tree=property,
            declarations=declarations,
            bound_vars=search_bounds,
            sampler=sampler,
        )
        counterexamples[property.name] = counterexample

    return counterexamples
