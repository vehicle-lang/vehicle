"""Shared helpers for backend-specific loss modules."""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
from typing import Any, Callable, Iterable, Mapping, MutableMapping, Protocol, cast

from ..loss import load_ast
from ..typing import DeclarationName, DifferentiableLogic


class _SamplerProtocol(Protocol):
    def get_loss(self, *args: Any, **kwargs: Any) -> Any: ...


TranslationFactory = Callable[[], Any]
SamplerFactory = Callable[[], _SamplerProtocol]
DomainValidator = Callable[[Any, Any], None]


def _validate_sampler(
    sampler: Callable[..., Any],
    domain_validator: DomainValidator,
) -> Callable[..., Any]:
    def validated_sampler(
        dims: Any,
        lower_bound: Any,
        upper_bound: Any,
        search_lambda: Any,
    ) -> Any:
        domain_validator(lower_bound, upper_bound)
        return sampler(dims, lower_bound, upper_bound, search_lambda)

    return validated_sampler


def load_loss_specification(
    path: str | Path,
    *,
    logic: DifferentiableLogic,
    samplers: Mapping[str, Any] | None,
    declarations: Iterable[DeclarationName],
    declaration_context: MutableMapping[str, Any] | None,
    translation_factory: TranslationFactory,
    default_sampler_factory: SamplerFactory,
    domain_validator: DomainValidator,
) -> dict[str, Any]:
    """Load a specification using the provided backend factories."""

    if declaration_context is None:
        declaration_context = {}

    if samplers is None:
        default_sampler = default_sampler_factory()
        validated_sampler = _validate_sampler(
            default_sampler.get_loss,
            domain_validator,
        )
        samplers = defaultdict(lambda: validated_sampler)
    else:
        samplers = {
            k: _validate_sampler(s.get_loss, domain_validator)
            for k, s in samplers.items()
        }

    program = load_ast(
        path,
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
