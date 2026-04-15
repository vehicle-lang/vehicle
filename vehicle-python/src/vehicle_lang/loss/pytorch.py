"""PyTorch-specific loss helpers."""

from __future__ import annotations

from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping

from ..typing import CustomLogic, DeclarationName, DifferentiableLogic, Target
from ._common import load_loss_specification
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
    logic: Target = DifferentiableLogic.DL2,
    temporal_semantics: Any | None = None,
    samplers: Mapping[str, Any] | None = None,
    declarations: Iterable[DeclarationName] = (),
    declaration_context: MutableMapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Load a loss function compiled for PyTorch.

    Args:
        path: Path to a Vehicle specification file.
        logic: The differentiable logic to use. Can be a built-in
            :class:`~vehicle_lang.typing.DifferentiableLogic` member or a
            :class:`~vehicle_lang.typing.CustomLogic` for user-defined logics.
        temporal_semantics: Optional :class:`vehicle_stl.Semantics` instance
            controlling how temporal operators (Globally, Finally, Until)
            interpret conjunction/disjunction.  Defaults to exact min/max.
        samplers: Custom samplers keyed by declaration name.
        declarations: Names of declarations to compile.
        declaration_context: Mutable context shared across declarations.
    """

    return load_loss_specification(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=lambda: PyTorchTranslation(
            temporal_semantics=temporal_semantics
        ),
        default_sampler_factory=DefaultPyTorchSampler,
    )
