"""Helpers for optional runtime dependencies."""

from importlib import import_module
from types import ModuleType


def require_optional_dependency(
    package_name: str,
    *,
    extra: str,
    feature: str,
) -> ModuleType:
    """Import an optional dependency or raise a helpful error."""
    try:
        return import_module(package_name)
    except ModuleNotFoundError as exc:  # pragma: no cover - clarity outweighs
        message = (
            f"{feature} requires the optional dependency '{package_name}'. "
            f'Install it with `pip install "vehicle_lang[{extra}]"`.'
        )
        raise ModuleNotFoundError(message) from exc
