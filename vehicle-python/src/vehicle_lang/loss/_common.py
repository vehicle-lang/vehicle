"""Shared helpers for backend-specific loss modules."""

from __future__ import annotations

from collections import defaultdict, namedtuple
from dataclasses import dataclass
from pathlib import Path
from typing import (
    Any,
    Callable,
    Iterable,
    Mapping,
    MutableMapping,
    Protocol,
    Sequence,
    cast,
)

from .._ast._nodes import Program
from ..loss import load_ast
from ..loss._python._translation import PythonTranslation
from ..typing import DeclarationName, DifferentiableLogic, LossMode
from ._search import Quantifiers, restructure_search_loss


class _SamplerProtocol(Protocol):
    def get_loss(self, *args: Any, **kwargs: Any) -> Any: ...


@dataclass
class SearchSpec:
    declarations: dict[str, Any]
    property_data: dict[str, Any]
    search_bounds: dict[str, Any]


TranslationFactory = Callable[[], Any]
SamplerFactory = Callable[[], _SamplerProtocol]

BoundVar = namedtuple("BoundVar", ["name", "lower_bound", "upper_bound"])


def load_loss_ast(
    path: str | Path,
    mode: LossMode,
    logic: DifferentiableLogic,
    declarations: Iterable[DeclarationName],
) -> Program:
    """Load a loss AST for training or search."""

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
    translator: PythonTranslation,
) -> dict[str, Any]:
    """Translate a loss function using a provided backend factory."""

    compiled = translator.compile_program(
        program=program,
        path=path,
        declaration_context=declaration_context,
        samplers=samplers,
    )
    return cast(dict[str, Any], compiled)


def translate_search_bounds(
    path: str | Path,
    quantifiers: dict[str, Quantifiers],
    declaration_context: MutableMapping[str, Any] | None,
    translator: PythonTranslation,
) -> dict[str, Sequence[BoundVar]]:
    """Translate quantifier bounds for search using a backend factory."""

    bound_var_data = {}
    for property_name, qs in quantifiers.items():
        bound_vars = []
        for q in qs:
            lower_bound = translator.compile_expression(
                expression=q.lower_bound,
                path=path,
                declaration_context=declaration_context,
            )
            upper_bound = translator.compile_expression(
                expression=q.upper_bound,
                path=path,
                declaration_context=declaration_context,
            )
            bound_var = BoundVar(
                name=q.name, lower_bound=lower_bound, upper_bound=upper_bound
            )
            bound_vars.append(bound_var)
        bound_var_data[property_name] = bound_vars

    return bound_var_data


def load_training_loss(
    path: str | Path,
    *,
    logic: DifferentiableLogic,
    samplers: Mapping[str, Any] | None,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,
    default_sampler_factory: SamplerFactory,
) -> dict[str, Any]:
    """Load a specification for training."""

    if declaration_context is None:
        declaration_context = {}

    if samplers is None:
        default_sampler = default_sampler_factory()
        samplers = defaultdict(lambda: default_sampler.get_loss)
    else:
        samplers = {k: s.get_loss for k, s in samplers.items()}

    program = load_loss_ast(
        path, mode=LossMode.Training, logic=logic, declarations=declarations
    )

    translator = translation_factory()
    compiled_declarations = translate_loss(
        path,
        program=program,
        samplers=samplers,
        declaration_context=declaration_context,
        translator=translator,
    )

    return compiled_declarations


def load_search_loss(
    path: str | Path,
    *,
    logic: DifferentiableLogic,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    networks: dict[DeclarationName, Any] = {},
    datasets: dict[DeclarationName, Any] = {},
    parameters: dict[DeclarationName, Any] = {},
    translation_factory: TranslationFactory,
) -> SearchSpec:
    """Load a specification for search."""

    if declaration_context is None:
        declaration_context = {}

    search_program = load_loss_ast(
        path, mode=LossMode.Search, logic=logic, declarations=declarations
    )

    quantifiers, restructured_program = restructure_search_loss(
        search_program.program,
        declaration_context=declaration_context,
        networks=networks,
        datasets=datasets,
        parameters=parameters,
    )

    translator = translation_factory()
    compiled_declarations = translate_loss(
        path,
        program=restructured_program,
        samplers={},
        declaration_context=declaration_context,
        translator=translator,
    )

    search_bounds = translate_search_bounds(
        path,
        quantifiers=quantifiers,
        declaration_context=declaration_context,
        translator=translator,
    )

    return SearchSpec(
        declarations=compiled_declarations,
        property_data=search_program.map,
        search_bounds=search_bounds,
    )
