"""Shared helpers for backend-specific loss modules."""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
from typing import Any, Callable, Iterable, Mapping, MutableMapping, Protocol, cast

from ..loss import load_ast
from ..typing import DeclarationName, DifferentiableLogic, LossMode

from .attack import extract_quantifiers

class _SamplerProtocol(Protocol):
    def get_loss(self, *args: Any, **kwargs: Any) -> Any: ...


TranslationFactory = Callable[[], Any]
SamplerFactory = Callable[[], _SamplerProtocol]


def load_loss_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic,
    samplers: Mapping[str, Any] | None,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,
    default_sampler_factory: SamplerFactory,
) -> dict[str, Any]:
    """Load a specification using the provided backend factories."""

    if declaration_context is None:
        declaration_context = {}

    if samplers is None:
        default_sampler = default_sampler_factory()
        samplers = defaultdict(lambda: default_sampler.get_loss)
    else:
        samplers = {k: s.get_loss for k, s in samplers.items()}

    program = load_ast(
        path,
        mode=LossMode.Training,
        target=logic,
        declarations=declarations,
    )

    translation = translation_factory()
    compiled = translation.compile(
        program=program,
        path=path,
        declaration_context=declaration_context,
        samplers=samplers,
    )
    return cast(dict[str, Any], compiled)


def load_loss_specification_quantifiers(
    path: str | Path,
    *,
    logic: DifferentiableLogic,
    samplers: Mapping[str, Any] | None,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,
    default_sampler_factory: SamplerFactory,  
) -> tuple[dict[str, Any], dict[str, Any], dict[str, Any]]:
    """
    Load a specification and extract quantifiers for search
    """
    if declaration_context is None:
        declaration_context = {}

    if samplers is None:
        default_sampler = default_sampler_factory()
        samplers = defaultdict(lambda: default_sampler.get_loss)
    else:
        samplers = {k: s.get_loss for k, s in samplers.items()}

    search_program = load_ast(
        path,
        mode=LossMode.Search,
        target=logic,
        declarations=declarations,
    )

    quantifiers = extract_quantifiers(search_program.program)

    translation = translation_factory()
    compiled = translation.compile(
        program=search_program.program,
        path=path,
        declaration_context=declaration_context,
        samplers=samplers,
    )

    print(search_program.map)

    return cast(dict[str, Any], compiled), quantifiers, search_program.map