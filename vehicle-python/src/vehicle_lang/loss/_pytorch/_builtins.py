from __future__ import annotations

from dataclasses import dataclass
from fractions import Fraction
from typing import TYPE_CHECKING, Any, Sequence, cast

from typing_extensions import override

from ..._deps import require_optional_dependency

if TYPE_CHECKING:
    import torch
else:  # pragma: no cover - exercised implicitly
    torch = require_optional_dependency(
        "torch",
        extra="pytorch",
        feature="The PyTorch loss backend",
    )

from .._abc import ABCBuiltins
from .._ast import _nodes
from ..error import VehicleInternalError  # type: ignore[attr-defined]

################################################################################
### Type-safe PyTorch wrappers
################################################################################


def _torch_tensor(*args: Any, **kwargs: Any) -> torch.Tensor:
    """Type-safe wrapper for torch.tensor that casts complex return type to torch.Tensor."""
    return cast(torch.Tensor, torch.tensor(*args, **kwargs))


################################################################################
### Interpretations of Vehicle builtins in PyTorch
################################################################################


@dataclass(frozen=True)
class PyTorchBuiltins(
    ABCBuiltins[
        int,
        float,
        torch.Tensor,
    ]
):
    dtype_index: torch.dtype = torch.int32
    dtype_rat: torch.dtype = torch.float32

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def RatTensor(self, value: _nodes.Tensor) -> torch.Tensor:
        match value.value:
            case Fraction():
                # Single value - expand to tensor shape
                float_value = float(value.value)
                return _torch_tensor(data=float_value, dtype=self.dtype_rat)
            case _:
                # Sequence of values
                return _torch_tensor(
                    data=tuple(float(val) for val in value.value),
                    dtype=self.dtype_rat,
                )

    @override
    def NegRatTensor(self, x: torch.Tensor) -> torch.Tensor:
        return torch.neg(x)

    @override
    def AddRatTensor(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.add(x, y)

    @override
    def SubRatTensor(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.sub(x, y)

    @override
    def MulRatTensor(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.mul(x, y)

    @override
    def DivRatTensor(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.div(x, y)

    @override
    def MinRatTensor(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.minimum(torch.as_tensor(x), torch.as_tensor(y))

    @override
    def MaxRatTensor(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.maximum(torch.as_tensor(x), torch.as_tensor(y))

    @override
    def ReduceAddRatTensor(
        self, e: float, xs: torch.Tensor | Sequence[torch.Tensor]
    ) -> torch.Tensor:
        xs = torch.stack(list(xs))
        return torch.add(torch.sum(xs), e)

    @override
    def ReduceMulRatTensor(
        self, e: float, x: torch.Tensor | Sequence[torch.Tensor]
    ) -> torch.Tensor:
        x = torch.stack(list(x))
        return torch.mul(torch.prod(x), e)

    @override
    def ReduceMinRatTensor(
        self, e: float, x: torch.Tensor | Sequence[torch.Tensor]
    ) -> torch.Tensor:
        x = torch.stack([torch.Tensor(e)] + list(x))
        return torch.min(x)

    @override
    def ReduceMaxRatTensor(
        self, e: float, x: torch.Tensor | Sequence[torch.Tensor]
    ) -> torch.Tensor:
        x = torch.stack([torch.Tensor(e)] + list(x))
        return torch.max(x)

    @override
    def DimensionLookup(
        self, xs: torch.Tensor | tuple[torch.Tensor, ...] | list[torch.Tensor], i: int
    ) -> torch.Tensor:
        # Despite the name, this implements element indexing (At operator in Haskell)
        # The JSON AST uses 'DimensionLookup' but semantics are element access

        # Handle tuple/sequence case (from StackTensor or similar)
        if isinstance(xs, (tuple, list)):
            return xs[i]

        if xs.ndim == 0:
            raise VehicleInternalError(
                "Cannot index into a scalar tensor in DimensionLookup, make an issue in GitHub."
            )

        # Use direct indexing which works for all tensor ranks >= 1
        return xs[i]

    @override
    def DimensionCons(self, head: int, tail: Sequence[int]) -> tuple[int, ...]:
        return (head, *tail)

    @override
    def DimensionNil(self) -> tuple[int, ...]:
        return ()

    @override
    def ConstTensor(self, value: float, shape: Sequence[int]) -> torch.Tensor:
        return torch.full(size=shape, fill_value=float(value), dtype=self.dtype_rat)

    @override
    def DenseTensor(
        self, values: Sequence[float], shape: Sequence[int]
    ) -> torch.Tensor:
        # Convert Fraction values to floats
        float_values = [float(val) for val in values]
        return _torch_tensor(data=float_values, dtype=self.dtype_rat).reshape(shape)

    @override
    def StackTensor(self, tensors: Sequence[torch.Tensor]) -> torch.Tensor:
        return torch.stack(cast(tuple[torch.Tensor], tensors))
