import ast as py
from dataclasses import dataclass
from fractions import Fraction
from pathlib import Path
from typing import Any, Callable, Iterable, Optional, Sequence, cast

import torch
from typing_extensions import TypeVar, override

from ...ast import Tensor
from ...ast import load as ast_load
from ...typing import (
    DeclarationName,
    DifferentiableLogic,
    Explicit,
    Target,
)
from ..abc import ABCBuiltins
from ..error import (
    VehiclePropertyNotCallable,
    VehiclePropertyNotFound,
)
from ..python import PythonTranslation
from . import types as vcl

# Create proper Python AST provenance (different from Vehicle provenance)
PY_MISSING = {"lineno": 0, "col_offset": 0}

################################################################################
### Type-safe PyTorch wrappers
################################################################################


def _torch_tensor(*args: Any, **kwargs: Any) -> torch.Tensor:
    """Type-safe wrapper for torch.tensor that casts complex return type to torch.Tensor."""
    return cast(torch.Tensor, torch.tensor(*args, **kwargs))


################################################################################
### Interpretations of Vehicle builtins in PyTorch
################################################################################

_T = TypeVar("_T")
_S = TypeVar("_S")


@dataclass(frozen=True)
class PyTorchBuiltins(
    ABCBuiltins[
        vcl.Index,
        vcl.Rat,
        torch.Tensor,
    ]
):
    dtype_index: torch.dtype = torch.int32
    dtype_rat: torch.dtype = torch.float32

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def RatTensor(self, value: Tensor) -> torch.Tensor:
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
        return torch.minimum(x, y)

    @override
    def MaxRatTensor(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        return torch.maximum(x, y)

    @override
    def ReduceAddRatTensor(self, e: float, xs: torch.Tensor) -> torch.Tensor:
        return torch.sum(xs)

    @override
    def ReduceMulRatTensor(self, e: float, x: torch.Tensor) -> torch.Tensor:
        return torch.prod(x)

    @override
    def ReduceMinRatTensor(self, e: float, x: torch.Tensor) -> torch.Tensor:
        return torch.min(x)

    @override
    def ReduceMaxRatTensor(self, e: float, x: torch.Tensor) -> torch.Tensor:
        return torch.max(x)

    @override
    def DimensionLookup(self, xs: torch.Tensor, i: vcl.Index) -> vcl.Index:
        return int(xs.shape[i])

    @override
    def DimensionCons(
        self, head: vcl.Index, tail: Sequence[vcl.Index]
    ) -> tuple[vcl.Index, ...]:
        return (head, *tail)

    @override
    def DimensionNil(self) -> tuple[vcl.Index, ...]:
        return ()

    @override
    def ConstTensor(self, value: vcl.Rat, shape: Sequence[vcl.Index]) -> torch.Tensor:
        return torch.full(size=shape, fill_value=value, dtype=self.dtype_rat)

    @override
    def DenseTensor(
        self, values: Sequence[vcl.Rat], shape: Sequence[vcl.Index]
    ) -> torch.Tensor:
        return _torch_tensor(data=list(values), dtype=self.dtype_rat).reshape(shape)

    @override
    def StackTensor(self, tensors: Sequence[torch.Tensor]) -> torch.Tensor:
        return torch.stack(cast(tuple[torch.Tensor], tensors))


################################################################################
### PyTorch Translation
################################################################################


@dataclass(frozen=True, init=False)
class PyTorchTranslation(PythonTranslation):
    def __init__(self) -> None:
        super().__init__(
            builtins=PyTorchBuiltins(),
            module_header=[
                py.Import(
                    names=[
                        py.alias(
                            name="torch",
                            asname=None,
                            lineno=0,
                            col_offset=0,
                        )
                    ],
                    lineno=0,
                    col_offset=0,
                )
            ],
        )


################################################################################
### Compilation utilities
################################################################################


def load(
    path: str | Path,
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = Explicit.Explicit,
    samplers: dict[str, vcl.ABCSampler],
    translation: Optional[PyTorchTranslation] = None,
) -> dict[str, Any]:
    if translation is None:
        translation = PyTorchTranslation()
    return translation.compile(
        ast_load(path, declarations=declarations, target=target),
        path=path,
        declaration_context={},
        samplers=samplers,
    )


def load_loss_function(
    path: str | Path,
    property_name: DeclarationName,
    *,
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
    samplers: dict[str, vcl.ABCSampler] = {},
) -> Callable[..., torch.Tensor]:
    """
    Load a loss function from a property in a Vehicle specification.

    :param path: The path to the Vehicle specification file.
    :param property_name: The name of the Vehicle property to load.
    :param target: The differentiable logic to use for interpreting the Vehicle property as a loss function, defaults to the Vehicle logic.
    :param samplers: A map from quantified variable names to samplers for their values. See `ABCSampler` for more details.
    :return: A function that takes the required external resources in the specification as keyword arguments and returns the loss corresponding to the property.
    """
    declarations = load(
        path, declarations=(property_name,), samplers=samplers, target=target
    )
    if property_name in declarations:
        property_func = declarations[property_name]
        if callable(property_func):
            return cast(Callable[..., torch.Tensor], property_func)
        else:
            raise VehiclePropertyNotCallable(property_name)
    else:
        raise VehiclePropertyNotFound(property_name)
