"""Shared helpers for backend-specific loss modules."""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Iterable, Mapping, MutableMapping, Protocol, cast

from ..loss import load_ast
from ..typing import DeclarationName, DifferentiableLogic, LossMode
from .._ast._nodes import Program

from .search import extract_quantifiers

class _SamplerProtocol(Protocol):
    def get_loss(self, *args: Any, **kwargs: Any) -> Any: ...

@dataclass
class TrainingSpec:
    declarations: dict[str, Any]

@dataclass
class SearchSpec:
    declarations: dict[str, Any]
    property_data: dict[str, Any]
    quantifiers: dict[str, Any]


TranslationFactory = Callable[[], Any]
SamplerFactory = Callable[[], _SamplerProtocol]

    
def load_loss_ast(
    path: str | Path,
    mode: LossMode,
    logic: DifferentiableLogic,
    declarations: Iterable[DeclarationName]
) -> Program:
    """Load a loss function AST for training or search."""
    
    program = load_ast(
        path,
        mode=mode,
        target=logic,
        declarations=declarations,
    )
    return program


def translate_loss(
    path: str | Path,
    program: Program,
    samplers: Mapping[str, Any] | None,
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,  
) -> dict[str, Any]:
    """Translate a loss function using a provided backend factory."""

    translation = translation_factory()
    compiled = translation.compile(
        program=program,
        path=path,
        declaration_context=declaration_context,
        samplers=samplers,
    )
    return cast(dict[str, Any], compiled)


def load_training_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic,
    samplers: Mapping[str, Any] | None,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,
    default_sampler_factory: SamplerFactory,
) -> TrainingSpec:
    """Load a specification for training."""

    if declaration_context is None:
        declaration_context = {}

    if samplers is None:
        default_sampler = default_sampler_factory()
        samplers = defaultdict(lambda: default_sampler.get_loss)
    else:
        samplers = {k: s.get_loss for k, s in samplers.items()}

    program = load_loss_ast(
        path,
        mode=LossMode.Training,
        logic=logic,
        declarations=declarations
    )

    declarations = translate_loss(
        path,
        program=program,
        samplers=samplers,
        declaration_context=declaration_context,
        translation_factory=translation_factory
    )

    return TrainingSpec(declarations)


def load_search_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic,
    samplers: Mapping[str, Any] | None,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,
    default_sampler_factory: SamplerFactory,  
) -> SearchSpec:
    """Load a specification for search."""

    if declaration_context is None:
        declaration_context = {}

    if samplers is None:
        default_sampler = default_sampler_factory()
        samplers = defaultdict(lambda: default_sampler.get_loss)
    else:
        samplers = {k: s.get_loss for k, s in samplers.items()}

    search_program = load_loss_ast(
        path,
        mode=LossMode.Search,
        logic=logic,
        declarations=declarations
    )

    declarations = translate_loss(
        path,
        program=search_program.program,
        samplers=samplers,
        declaration_context=declaration_context,
        translation_factory=translation_factory
    )

    quantifiers = extract_quantifiers(search_program.program)

    return SearchSpec(declarations=declarations,
                      property_data=search_program.map,
                      quantifiers=quantifiers)