from fractions import Fraction
from dataclasses import dataclass
from typing import Sequence, Any, cast
from typing_extensions import override
import torch
from ..abc import ABCBuiltins
from ..ast import nodes

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
    def RatTensor(self, value: nodes.Tensor) -> torch.Tensor:
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
    def ReduceAddRatTensor(self, e: float, xs: torch.Tensor | Sequence[torch.Tensor]) -> torch.Tensor:
        # e is the identity element (0 for addition), xs is the samples to reduce
        if isinstance(xs, (list, tuple)):
            xs = torch.stack(list(xs))
        return torch.sum(xs)

    @override
    def ReduceMulRatTensor(self, e: float, x: torch.Tensor | Sequence[torch.Tensor]) -> torch.Tensor:
        # e is the identity element (1 for multiplication), x is the samples to reduce
        if isinstance(x, (list, tuple)):
            x = torch.stack(list(x))
        return torch.prod(x)

    @override
    def ReduceMinRatTensor(self, e: float, x: torch.Tensor | Sequence[torch.Tensor]) -> torch.Tensor:
        # e is the identity element, x is the samples to reduce
        if isinstance(x, (list, tuple)):
            x = torch.stack(list(x))
        return torch.min(x)

    @override
    def ReduceMaxRatTensor(self, e: float, x: torch.Tensor | Sequence[torch.Tensor]) -> torch.Tensor:
        # e is the identity element, x is the samples to reduce
        if isinstance(x, (list, tuple)):
            x = torch.stack(list(x))
        return torch.max(x)

    @override
    def DimensionLookup(self, xs: torch.Tensor, i: int) -> torch.Tensor:
        # Despite the name, this implements element indexing (At operator in Haskell)
        # The JSON AST uses 'DimensionLookup' but semantics are element access
        
        # Handle tuple/sequence case (from StackTensor or similar)
        if isinstance(xs, (tuple, list)):
            return xs[i]
        
        # Handle scalar tensor case - can't be indexed
        if xs.ndim == 0:
            return xs
            
        # Use direct indexing which works for all tensor ranks >= 1
        return xs[i]

    @override
    def DimensionCons(
        self, head: int, tail: Sequence[int]
    ) -> tuple[int, ...]:
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
