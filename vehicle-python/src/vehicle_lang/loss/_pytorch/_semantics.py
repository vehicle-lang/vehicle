"""Helpers for constructing vehicle_stl.Semantics from compiled DL fields."""

from __future__ import annotations

from collections.abc import Callable
from typing import TYPE_CHECKING, Any

from vehicle_stl.semantics import ReductionOp

if TYPE_CHECKING:
    import torch


def lift_to_reduction(
    binary_op: Callable[["torch.Tensor", "torch.Tensor"], "torch.Tensor"],
    identity: float,
) -> ReductionOp:
    """Lift a binary tensor op to a fold-based ReductionOp for vehicle-stl.

    Converts a binary function ``op(acc, s)`` into a reduction
    ``reduce(op, signal.unbind(dim), identity)`` that vehicle-stl can use as
    a temporal conjunction or disjunction.

    The fold is differentiable: autograd traces through the loop.
    """

    def reduction(
        signal: "torch.Tensor",
        dim: int = 0,
        keepdim: bool = True,
        **kwargs: Any,
    ) -> "torch.Tensor":
        import torch

        slices = signal.unbind(dim)
        acc = torch.full_like(slices[0], identity)
        for s in slices:
            acc = binary_op(acc, s)
        return acc.unsqueeze(dim) if keepdim else acc

    return reduction
