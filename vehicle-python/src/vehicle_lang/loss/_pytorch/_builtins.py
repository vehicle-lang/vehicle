from __future__ import annotations

from dataclasses import dataclass, field
from fractions import Fraction
from typing import TYPE_CHECKING, Any, Sequence, cast

from typing_extensions import override

from ..._deps import require_optional_dependency

if TYPE_CHECKING:
    import torch
    import vehicle_stl
else:  # pragma: no cover - exercised implicitly
    torch = require_optional_dependency(
        "torch",
        extra="pytorch",
        feature="The PyTorch loss backend",
    )
    vehicle_stl = require_optional_dependency(
        "vehicle_stl",
        extra="pytorch",
        feature="Temporal operators in the PyTorch loss backend",
    )

from .._abc import ABCBuiltins
from .._ast import _nodes
from ..error import VehicleInternalError

################################################################################
### Type-safe PyTorch wrappers
################################################################################


def _torch_tensor(*args: Any, **kwargs: Any) -> torch.Tensor:
    """Type-safe wrapper for torch.tensor that casts complex return type to torch.Tensor."""
    return cast(torch.Tensor, torch.tensor(*args, **kwargs))


def _tensor_content_key(
    t: torch.Tensor,
) -> tuple[int, tuple[int, ...], tuple[int, ...], int, torch.dtype, torch.device]:
    return (
        t.data_ptr(),
        tuple(t.shape),
        tuple(t.stride()),
        cast(int, t.storage_offset()),
        t.dtype,
        t.device,
    )


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
    temporal_semantics: Any | None = None
    _formula_cache: dict[tuple[str, int, int], Any] = field(
        default_factory=dict, repr=False, compare=False, hash=False
    )
    _rollout_cache: dict[
        tuple[
            int,
            int,
            int,
            tuple[
                int, tuple[int, ...], tuple[int, ...], int, torch.dtype, torch.device
            ],
        ],
        torch.Tensor,
    ] = field(default_factory=dict, repr=False, compare=False, hash=False)

    def _clear_rollout_cache(self) -> None:
        self._rollout_cache.clear()

    def _get_formula(self, kind: str, start: int, end: int) -> Any:
        """Get or create a cached vehicle-stl formula for the given operator and interval."""
        key = (kind, start, end)
        if key not in self._formula_cache:
            sem = self.temporal_semantics
            kwargs: dict[str, Any] = {"interval": [start, end]}
            if sem is not None:
                kwargs["semantics"] = sem
            if kind == "always":
                self._formula_cache[key] = vehicle_stl.Always(**kwargs)
            elif kind == "eventually":
                self._formula_cache[key] = vehicle_stl.Eventually(**kwargs)
            elif kind == "until":
                self._formula_cache[key] = vehicle_stl.Until(**kwargs)
        return self._formula_cache[key]

    def _validate_temporal_interval(self, start: int, end: int) -> tuple[int, int]:
        start_idx = int(start)
        end_idx = int(end)
        if start_idx < 0:
            raise VehicleInternalError(
                f"Temporal operator interval start must be non-negative, found {start_idx}."
            )
        if end_idx < start_idx:
            raise VehicleInternalError(
                f"Temporal operator interval must satisfy start <= end, found [{start_idx},{end_idx}]."
            )
        return start_idx, end_idx

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
    def Globally(self, start: int, end: int, x: torch.Tensor) -> torch.Tensor:
        start_idx, end_idx = self._validate_temporal_interval(start, end)
        return self._get_formula("always", start_idx, end_idx)(x)

    @override
    def Finally(self, start: int, end: int, x: torch.Tensor) -> torch.Tensor:
        start_idx, end_idx = self._validate_temporal_interval(start, end)
        return self._get_formula("eventually", start_idx, end_idx)(x)

    @override
    def Until(
        self, start: int, end: int, x: torch.Tensor, y: torch.Tensor
    ) -> torch.Tensor:
        start_idx, end_idx = self._validate_temporal_interval(start, end)
        if x.shape != y.shape:
            raise VehicleInternalError(
                "Temporal Until expects both traces to have the same shape."
            )
        return self._get_formula("until", start_idx, end_idx)((x, y))

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
    def Rollout(
        self,
        n: int,
        controller: Any,
        dynamics: Any,
        init_state: torch.Tensor,
    ) -> torch.Tensor:
        key = (n, id(controller), id(dynamics), _tensor_content_key(init_state))
        cached = self._rollout_cache.get(key)
        if cached is not None:
            return cached
        states = [init_state]
        for _ in range(n - 1):
            action = controller(states[-1])
            next_state = dynamics(states[-1], action)
            states.append(next_state)
        result = torch.stack(states)
        self._rollout_cache[key] = result
        return result

    @override
    def ForeachTensor(self, dim: int, fn: Any) -> torch.Tensor:
        return torch.stack([fn(i) for i in range(dim)])

    @override
    def Transpose(self, xs: torch.Tensor) -> torch.Tensor:
        return xs.permute(*reversed(range(xs.ndim)))

    @override
    def StackTensor(self, tensors: Sequence[torch.Tensor]) -> torch.Tensor:
        return torch.stack(cast(tuple[torch.Tensor], tensors))
