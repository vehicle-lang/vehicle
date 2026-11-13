from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Callable, List, Sequence, Tuple, cast

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

from ..._ast import _nodes
from ...error import VehicleInternalError
from .._abc import ABCBuiltins

################################################################################
### Type-safe PyTorch wrappers
################################################################################


def _torch_tensor(*args: Any, **kwargs: Any) -> torch.Tensor:
    """Type-safe wrapper for torch.tensor that casts complex return type to torch.Tensor."""
    return cast(torch.Tensor, torch.tensor(*args, **kwargs))


def _extended_rational_to_float(value: _nodes.ExtendedFraction) -> float:
    match value:
        case _nodes.Finite(value=inner):
            return float(inner)
        case _nodes.PosInfinity():
            return float("inf")
        case _nodes.NegInfinity():
            return float("-inf")
        case _:
            raise ValueError(f"Unknown extended rational type: {type(value)}")


def _value_to_bool(value: object) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, _nodes.ExtendedFraction):
        return bool(_extended_rational_to_float(value))
    return bool(value)


def _comparison(op: str, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
    match op:
        case "Eq":
            return torch.eq(x, y)
        case "Ne":
            return torch.ne(x, y)
        case "Le":
            return torch.le(x, y)
        case "Lt":
            return torch.lt(x, y)
        case "Ge":
            return torch.ge(x, y)
        case "Gt":
            return torch.gt(x, y)
        case _:
            raise VehicleInternalError(f"Unknown comparison operation: {op}")


################################################################################
### Interpretations of Vehicle builtins in PyTorch
################################################################################


@dataclass(frozen=True)
class PyTorchBuiltins(
    ABCBuiltins[
        int,
        float,
        torch.Tensor,
        List[Any],
    ]
):
    dtype_index: torch.dtype = torch.int32
    dtype_rat: torch.dtype = torch.float32

    @override
    def BoolTensor(self, x: _nodes.Tensor[bool]) -> torch.Tensor:
        match x:
            case _nodes.DenseTensor():
                values = x.values
            case _nodes.ConstantTensor():
                values = (x.value,)
            case _:
                raise VehicleInternalError(f"Unknown tensor type: {type(x)}.")

        return _torch_tensor(data=values, dtype=torch.bool).reshape(x.shape)

    @override
    def BoolNot(self, x: torch.Tensor) -> torch.Tensor:
        return torch.logical_not(x)

    @override
    def BoolAnd(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.logical_and(x, y)

    @override
    def BoolOr(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.logical_or(x, y)

    @override
    def BoolImplies(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.logical_or(torch.logical_not(x), y)

    @override
    def BoolCompareIndex(self, op: str, x: int, y: int) -> torch.Tensor:
        tx = _torch_tensor(data=x, dtype=self.dtype_index)
        ty = _torch_tensor(data=y, dtype=self.dtype_index)
        return _comparison(op, tx, ty)

    @override
    def BoolCompareNat(self, op: str, x: int, y: int) -> torch.Tensor:
        tx = _torch_tensor(data=x, dtype=self.dtype_index)
        ty = _torch_tensor(data=y, dtype=self.dtype_index)
        return _comparison(op, tx, ty)

    @override
    def BoolCompareRatTensor(
        self,
        op: str,
        pointwise_dims: Sequence[int],
        reduce_dims: Sequence[int],
        x: torch.Tensor,
        y: torch.Tensor,
    ) -> torch.Tensor:
        _ = pointwise_dims
        result = _comparison(op, torch.as_tensor(x), torch.as_tensor(y))
        for _ in reduce_dims:
            result = torch.all(result, dim=-1)
        return result

    @override
    def BoolReduceAnd(self, x: torch.Tensor) -> torch.Tensor:
        return torch.all(x)

    @override
    def BoolReduceOr(self, x: torch.Tensor) -> torch.Tensor:
        return torch.any(x)

    @override
    def BoolIf(self, c: torch.Tensor, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.where(c, x, y)

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def RatTensor(self, x: _nodes.Tensor[_nodes.ExtendedFraction]) -> torch.Tensor:
        match x:
            case _nodes.DenseTensor():
                values = tuple(_extended_rational_to_float(val) for val in x.values)
            case _nodes.ConstantTensor():
                values = (_extended_rational_to_float(x.value),)
            case _:
                raise VehicleInternalError(f"Unknown tensor type: {type(x)}.")

        return _torch_tensor(data=values, dtype=self.dtype_rat).reshape(x.shape)

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
    def PowRatTensor(self, x: torch.Tensor, y: float) -> torch.Tensor:
        return torch.pow(torch.as_tensor(x), torch.as_tensor(y))

    @override
    def LogRatTensor(self, x: torch.Tensor) -> torch.Tensor:
        return torch.log(torch.as_tensor(x))

    @override
    def ExpRatTensor(self, x: torch.Tensor) -> torch.Tensor:
        return torch.exp(torch.as_tensor(x))

    @override
    def ReduceAddRatTensor(self, xs: torch.Tensor) -> torch.Tensor:
        return torch.sum(xs)

    @override
    def ReduceMulRatTensor(self, x: torch.Tensor) -> torch.Tensor:
        return torch.prod(x)

    @override
    def ReduceMinRatTensor(self, x: torch.Tensor) -> torch.Tensor:
        return torch.min(x)

    @override
    def ReduceMaxRatTensor(self, x: torch.Tensor) -> torch.Tensor:
        return torch.max(x)

    @override
    def WhereTensor(
        self, input: torch.Tensor, condition: torch.Tensor, false_value: torch.Tensor
    ) -> torch.Tensor:
        return torch.where(condition=condition, input=input, other=false_value)

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
    def Transpose(self, xs: torch.Tensor) -> torch.Tensor:
        # Note: torch.transpose only works for 2D tensors, so we use permute for generality
        return xs.permute(*reversed(range(xs.ndim)))

    @override
    def StackTensor(self, tensors: Sequence[torch.Tensor]) -> torch.Tensor:
        return torch.stack(cast(tuple[torch.Tensor], tensors))

    @override
    def AtTensor(
        self, xs: torch.Tensor | tuple[torch.Tensor, ...] | list[torch.Tensor], i: int
    ) -> torch.Tensor:
        if isinstance(xs, torch.Tensor) and xs.ndim == 0:
            raise VehicleInternalError(
                "Cannot index into a scalar tensor in AtTensor, make an issue in GitHub."
            )

        return xs[i]

    @override
    def ForeachTensor(
        self, size: int, function: Callable[[int], torch.Tensor]
    ) -> torch.Tensor:
        # Apply the function to each index and stack the results
        return torch.stack([function(i) for i in range(size)])

    @override
    def VectorLiteral(self, xs: Sequence[Any]) -> List[Any]:
        return list(xs)

    @override
    def AtVector(self, xs: torch.Tensor | Tuple[Any, ...] | List[Any], i: int) -> Any:
        if isinstance(xs, torch.Tensor) and xs.ndim == 0:
            raise VehicleInternalError(
                "Cannot index into a scalar tensor in AtVector, make an issue in GitHub."
            )

        # Use direct indexing which works for all tensor ranks >= 1
        return xs[i]

    @override
    def ForeachVector(self, size: int, function: Callable[[int], Any]) -> List[Any]:
        # Apply the function to each index and stack the results
        return [function(i) for i in range(size)]
