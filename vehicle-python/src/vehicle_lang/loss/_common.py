"""Shared helpers for backend-specific loss modules."""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
from types import ModuleType
from typing import Any, Callable, Iterable, Mapping, MutableMapping, Protocol, cast

from .._ast import _nodes as vcl
from ..loss import load_ast
from ..typing import DeclarationName, DifferentiableLogic
from . import _records


class _SamplerProtocol(Protocol):
    def get_loss(self, *args: Any, **kwargs: Any) -> Any: ...


TranslationFactory = Callable[..., Any]
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
    types: ModuleType | None = None,
    adapt_networks: bool = True,
    records_supported: bool = True,
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
        target=logic,
        declarations=declarations,
    )

    record_types = _resolve_record_types(path, program, types, records_supported)
    if record_types is not None:
        declaration_context["__vehicle_record_types__"] = record_types

    translation = translation_factory(adapt_networks=adapt_networks)
    compiled = translation.compile(
        program=program,
        path=path,
        declaration_context=declaration_context,
        samplers=samplers,
    )
    return cast(dict[str, Any], compiled)


def _resolve_record_types(
    path: str | Path,
    program: vcl.Program,
    types: ModuleType | None,
    records_supported: bool,
) -> ModuleType | None:
    schemas = _records.schemas_of(program)
    if not schemas:
        return None
    if not records_supported:
        raise RuntimeError(
            f"spec {path} declares @tensor record(s) "
            f"{[s.name for s in schemas]}; records are only supported on the "
            f"PyTorch backend"
        )
    if types is None:
        return _records.build_record_classes(schemas, path)

    expected = _records.schema_digest(schemas)
    actual = getattr(types, _records.DIGEST_ATTR, None)
    if actual != expected:
        raise RuntimeError(
            f"types module {types.__name__!r} was generated from a different version "
            f"of {path} (schema digest {actual!r}, expected {expected!r}); "
            f"regenerate via 'vehicle compile python-types -s {path} -o <out>.py'"
        )
    return types
