"""Shared helpers for backend-specific loss modules."""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
from typing import Any, Callable, Iterable, Mapping, MutableMapping, Protocol, cast

from ..typing import DeclarationName, Target
from . import _ast


class _SamplerProtocol(Protocol):
    def get_loss(self, *args: Any, **kwargs: Any) -> Any: ...


TranslationFactory = Callable[[], Any]
SamplerFactory = Callable[[], _SamplerProtocol]


def load_loss_specification(
    path: str | Path,
    *,
    logic: Target,
    samplers: Mapping[str, Any] | None,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,
    default_sampler_factory: SamplerFactory,
    _program: Any | None = None,
) -> dict[str, Any]:
    """Load a specification using the provided backend factories.

    Returns:
        The compiled callables. The Vehicle compiler emits each property as a
        minimisation target by default — wrapping properties in ``not`` so
        reducing the output always pushes the property toward satisfaction.
        Pass ``--dl-native-direction`` through to the compiler if you need
        the raw DL-native form.

    Args:
        _program: Internal — if provided, skip calling ``_ast.load`` and use
            this pre-loaded program directly. Allows callers that have already
            loaded the program (e.g., to derive temporal semantics) to avoid a
            second compiler invocation.
    """

    if declaration_context is None:
        declaration_context = {}

    if samplers is None:
        default_sampler = default_sampler_factory()
        samplers = defaultdict(lambda: default_sampler.get_loss)

    program = (
        _program
        if _program is not None
        else _ast.load(
            path,
            target=logic,
            declarations=declarations,
        )
    )

    if not isinstance(program, _ast._nodes.Main):
        raise TypeError(f"Expected Main program node, got {type(program).__name__}")

    translation = translation_factory()
    compiled = translation.compile(
        program=program,
        path=path,
        declaration_context=declaration_context,
        samplers=samplers,
    )
    return cast(dict[str, Any], compiled)
