"""Backend-specific helpers for Vehicle loss compilation."""

from typing import TYPE_CHECKING

__all__ = ["tensorflow", "pytorch"]

if TYPE_CHECKING:  # pragma: no cover - import-time only typing aid
    from . import pytorch as pytorch
    from . import tensorflow as tensorflow
