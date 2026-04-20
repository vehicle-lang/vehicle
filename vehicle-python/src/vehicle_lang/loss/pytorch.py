"""PyTorch-specific loss helpers."""

from __future__ import annotations

import ast as py
from fractions import Fraction
from pathlib import Path
from typing import Any, Iterable, Mapping, MutableMapping, cast

from ..typing import CustomLogic, DeclarationName, DifferentiableLogic, Target
from . import _ast as _loss_ast
from ._ast import _nodes as vcl
from ._common import load_loss_specification
from ._pytorch._builtins import PyTorchBuiltins
from ._pytorch._semantics import lift_to_reduction
from ._pytorch._translation import PyTorchTranslation
from ._pytorch.samplers import DefaultPyTorchSampler, PyTorchSampler

__all__ = [
    "load_specification",
    "PyTorchSampler",
    "DefaultPyTorchSampler",
]


def _extract_scalar(expr: vcl.Expression) -> float:
    """Extract a float from a compiled 0-dimensional RatTensor expression."""
    if not isinstance(expr, vcl.RatTensor):
        raise ValueError(
            f"Expected RatTensor for identity field, got {type(expr).__name__}"
        )
    tensor = expr.contents
    if isinstance(tensor, vcl.ConstantTensor):
        return float(tensor.value)
    elif isinstance(tensor, vcl.DenseTensor):
        return float(tensor.value[0])
    else:
        raise ValueError(f"Unknown tensor type: {type(tensor).__name__}")


def _derive_temporal_semantics(program: vcl.Program) -> Any:
    """Derive a vehicle_stl.Semantics from the temporal metadata in a compiled program."""
    import torch
    import vehicle_stl

    if not isinstance(program, vcl.Main):
        raise ValueError("Expected Main program node")

    meta = program.temporal_semantics

    # Build a throwaway translation to compile the VCL lambda AST nodes
    translation = PyTorchTranslation()

    if not isinstance(meta.conjunction, vcl.Lam):
        raise ValueError(
            f"Expected Lam for conjunction, got {type(meta.conjunction).__name__}"
        )
    if not isinstance(meta.disjunction, vcl.Lam):
        raise ValueError(
            f"Expected Lam for disjunction, got {type(meta.disjunction).__name__}"
        )

    conj_lambda_ast = translation.translate_binary_function(meta.conjunction)
    disj_lambda_ast = translation.translate_binary_function(meta.disjunction)

    builtins = translation.builtins
    scope = {"torch": torch, "__vehicle__": builtins}
    conj_fn = eval(
        compile(
            py.fix_missing_locations(py.Expression(body=conj_lambda_ast)),
            "<conj>",
            "eval",
        ),
        scope,
    )
    disj_fn = eval(
        compile(
            py.fix_missing_locations(py.Expression(body=disj_lambda_ast)),
            "<disj>",
            "eval",
        ),
        scope,
    )

    conj_id = _extract_scalar(meta.conjunction_identity)
    disj_id = _extract_scalar(meta.disjunction_identity)

    return vehicle_stl.Semantics(
        conjunction=lift_to_reduction(conj_fn, conj_id),
        disjunction=lift_to_reduction(disj_fn, disj_id),
        conjunction_identity=conj_id,
        disjunction_identity=disj_id,
    )


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
            interpret conjunction/disjunction.  If ``None`` and the logic has
            temporal semantics defined (e.g. STLLoss), they are derived
            automatically from the compiled differentiable logic.
        samplers: Custom samplers keyed by declaration name.
        declarations: Names of declarations to compile.
        declaration_context: Mutable context shared across declarations.
    """
    # Load the program once so we can derive temporal semantics if needed.
    program = _loss_ast.load(path, target=logic, declarations=declarations)

    if temporal_semantics is None:
        temporal_semantics = _derive_temporal_semantics(program)

    translation_holder: dict[str, PyTorchTranslation] = {}

    def _factory() -> PyTorchTranslation:
        translation = PyTorchTranslation(temporal_semantics=temporal_semantics)
        translation_holder["translation"] = translation
        return translation

    compiled = load_loss_specification(
        path,
        logic=logic,
        samplers=samplers,
        declarations=declarations,
        declaration_context=declaration_context,
        translation_factory=_factory,
        default_sampler_factory=DefaultPyTorchSampler,
        _program=program,
    )

    builtins = cast(PyTorchBuiltins, translation_holder["translation"].builtins)

    def _wrap(fn: Any) -> Any:
        def wrapped(*args: Any, **kwargs: Any) -> Any:
            builtins._clear_rollout_cache()
            try:
                return fn(*args, **kwargs)
            finally:
                builtins._clear_rollout_cache()

        return wrapped

    return {
        name: (_wrap(value) if callable(value) else value)
        for name, value in compiled.items()
    }
