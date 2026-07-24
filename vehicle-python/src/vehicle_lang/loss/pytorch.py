"""PyTorch-specific loss helpers."""

from __future__ import annotations
from dataclasses import dataclass

from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping, List

from ..typing import DeclarationName, DifferentiableLogic, DL2DifferentiableLogic
from .._ast._nodes import SearchRatTensor
from ._common import load_training_loss, load_search_loss, TrainingSpec, SearchSpec
from ._pytorch._translation import PyTorchTranslation
from ._pytorch.samplers import DefaultPyTorchSampler, PyTorchSampler
from ._pytorch.search import pgd, Sample
import torch

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

    training_spec = load_training_loss(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=PyTorchTranslation,
        default_sampler_factory=DefaultPyTorchSampler,
    )
    return training_spec.declarations


def load_search_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic = DL2DifferentiableLogic(),
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
    networks: dict[DeclarationName, Any] = {},
    datasets: dict[DeclarationName, Any] = {},
    parameters: dict[DeclarationName, Any] = {}
) -> SearchSpec:

    return load_search_loss(
        path,
        logic=logic,
        declarations=declarations,
        declaration_context=declaration_context,
        networks=networks,
        datasets=datasets,
        parameters=parameters,
        translation_factory=PyTorchTranslation
    )


def search(
    quantifier_data: List[Any],
    loss_fn: Any,
    num_samples: int = 10,
    num_steps: int = 5, # number of steps per quantified variable
    seed: int | None = None
) -> List[Sample]:
    
    if seed is not None:
        torch.manual_seed(seed)
    
    samples = []
    for _ in range(num_samples):
        sample = pgd(quantifier_data, loss_fn, num_steps)
        samples.append(sample)
    return samples