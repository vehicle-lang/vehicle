"""Shared helpers for backend-specific loss modules."""

from __future__ import annotations

from collections import defaultdict
from pathlib import Path
from types import ModuleType
from typing import Any, Callable, Iterable, Mapping, MutableMapping, Protocol, cast

from .._ast import _nodes as vcl
from ..loss import load_ast
from ..typing import DeclarationName, DifferentiableLogic


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
    types: ModuleType | None = None,
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

    _check_types_module(path, program, types)
    if types is not None:
        declaration_context["__vehicle_record_types__"] = types

    translation = translation_factory()
    compiled = translation.compile(
        program=program,
        path=path,
        declaration_context=declaration_context,
        samplers=samplers,
    )
    return cast(dict[str, Any], compiled)


def _check_types_module(
    path: str | Path,
    program: vcl.Program,
    types: ModuleType | None,
) -> None:
    """Raise if the spec declares @tensor records but no types module is provided."""
    if not isinstance(program, vcl.Main):
        return
    schemas = [d for d in program.declarations if isinstance(d, vcl.DefRecordSchema)]
    if not schemas:
        return
    names = [s.name for s in schemas]
    if types is None:
        raise RuntimeError(
            f"spec {path} declares @tensor record(s) {names}; "
            f"run 'vehicle compile python-types -s {path} -o <out>.py' "
            f"and pass 'types=<imported module>' to load_specification"
        )
    missing = [n for n in names if not hasattr(types, n)]
    if missing:
        raise RuntimeError(
            f"types module {types.__name__!r} is missing class(es) {missing}; "
            f"regenerate via 'vehicle compile python-types -s {path}'"
        )
