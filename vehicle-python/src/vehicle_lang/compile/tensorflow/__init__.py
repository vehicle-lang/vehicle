import ast as py
from dataclasses import dataclass
from fractions import Fraction
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, Optional, Tuple, cast

import tensorflow as tf  # type: ignore[import-untyped,unused-ignore]
from typing_extensions import TypeVar, override

from ...ast import Tensor
from ...ast import load as ast_load
from ...typing import (
    AnyOptimisers,
    DeclarationName,
    DifferentiableLogic,
    Explicit,
    Target,
)
from ..abc import ABCBuiltins
from ..error import (
    VehicleBuiltinUnsupported,
    VehiclePropertyNotCallable,
    VehiclePropertyNotFound,
)
from ..python import PythonTranslation
from . import types as vcl

# Create proper Python AST provenance (different from Vehicle provenance)
PY_MISSING = {"lineno": 0, "col_offset": 0}

################################################################################
### Type-safe TensorFlow wrappers
################################################################################


def _tf_constant(*args: Any, **kwargs: Any) -> tf.Tensor:
    """Type-safe wrapper for tf.constant that casts complex return type to tf.Tensor."""
    return cast(tf.Tensor, tf.constant(*args, **kwargs))


def _tf_map_fn(*args: Any, **kwargs: Any) -> tf.Tensor:
    """Type-safe wrapper for tf.map_fn that casts complex return type to tf.Tensor."""
    return cast(tf.Tensor, tf.map_fn(*args, **kwargs))


################################################################################
### Interpretations of Vehicle builtins in Tensorflow
################################################################################

_T = TypeVar("_T")


@dataclass(frozen=True)
class TensorFlowBuiltins(
    ABCBuiltins[
        vcl.Index,
        vcl.Bool,
        vcl.Nat,
        vcl.Int,
        vcl.Rat,
        tf.Tensor,
    ]
):
    dtype_index: tf.DType = tf.uint32
    dtype_bool: tf.DType = tf.bool
    dtype_nat: tf.DType = tf.uint32
    dtype_int: tf.DType = tf.int32
    dtype_rat: tf.DType = tf.float32

    @override
    def BoolTensor(self, value: Tensor) -> tf.Tensor:
        return _tf_constant(value=value.value, dtype=self.dtype_bool, shape=value.shape)

    @override
    def NatTensor(self, value: Tensor) -> tf.Tensor:
        return _tf_constant(value=value.value, dtype=self.dtype_nat, shape=value.shape)

    @override
    def IntTensor(self, value: Tensor) -> tf.Tensor:
        return _tf_constant(value=value.value, dtype=self.dtype_int, shape=value.shape)

    @override
    def RatTensor(self, value: Tensor) -> tf.Tensor:
        match value.value:
            case Fraction():
                # Single value - expand to tensor shape
                float_value = float(value.value)
                return _tf_constant(
                    value=float_value, dtype=self.dtype_rat, shape=value.shape
                )
            case _:
                # Sequence of values
                return _tf_constant(
                    value=tuple(float(val) for val in value.value),
                    dtype=self.dtype_rat,
                    shape=value.shape,
                )

    @override
    def NotBoolTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.logical_not(x)

    @override
    def AndBoolTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.logical_and(x, y)

    @override
    def OrBoolTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.logical_or(x, y)

    @override
    def NegRatTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.negative(x)

    @override
    def AddRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.add(x, y)

    @override
    def SubRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    @override
    def MulRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.multiply(x, y)

    @override
    def DivRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.divide(x, y)

    @override
    def EqRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.equal(x, y)

    @override
    def NeRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.not_equal(x, y)

    @override
    def LeRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.less_equal(x, y)

    @override
    def LtRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.less(x, y)

    @override
    def GeRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.greater_equal(x, y)

    @override
    def GtRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.greater(x, y)

    @override
    def PowRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.pow(x, y)

    @override
    def MinRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.minimum(x, y)

    @override
    def MaxRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.maximum(x, y)

    @override
    def ReduceAndBoolTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_all(x)

    @override
    def ReduceOrBoolTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_any(x)

    @override
    def ReduceSumRatTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_sum(x)

    @override
    def ReduceRatTensor(
        self,
        f: Callable[[tf.Tensor, tf.Tensor], tf.Tensor],
        x: tf.Tensor,
    ) -> tf.Tensor:
        return tf.foldr(f, x)

    @override
    def EqIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x == y

    @override
    def NeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x != y

    @override
    def LeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x <= y

    @override
    def LtIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x < y

    @override
    def GeIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x >= y

    @override
    def GtIndex(self, x: vcl.Index, y: vcl.Index) -> vcl.Bool:
        return x > y

    @override
    def LookupRatTensor(self, x: tf.Tensor, i: tf.Tensor) -> vcl.Rat:
        return cast(vcl.Rat, tf.gather(x, i))

    @override
    def StackRatTensor(self, n: int, *xs: tf.Tensor) -> tf.Tensor:
        return tf.stack(values=xs)

    @override
    def ConstRatTensor(self, value: vcl.Rat) -> tf.Tensor:
        return _tf_constant(value=value, dtype=self.dtype_rat)

    @override
    def MapRatTensor(self, f: Callable[[vcl.Rat], vcl.Rat], x: tf.Tensor) -> tf.Tensor:
        return _tf_map_fn(f, x, dtype=self.dtype_rat)

    @override
    def ZipWithRatTensor(
        self,
        f: Callable[[vcl.Rat, vcl.Rat], vcl.Rat],
        x: tf.Tensor,
        y: tf.Tensor,
    ) -> tf.Tensor:
        return _tf_map_fn(
            lambda xy: f(xy[0], xy[1]),
            tf.stack(
                (
                    x,
                    y,
                ),
                axis=1,
            ),
        )

    @override
    def IndicesIndexTensor(self, x: tf.Tensor) -> tf.Tensor:
        return x

    @override
    def MinimiseRatTensor(
        self,
        join: Callable[[tf.Tensor, tf.Tensor], tf.Tensor],
        predicate: Callable[..., tf.Tensor],
    ) -> tf.Tensor:
        raise VehicleBuiltinUnsupported("MinimiseRatTensor")

    @override
    def MaximiseRatTensor(
        self,
        meet: Callable[[tf.Tensor, tf.Tensor], tf.Tensor],
        predicate: Callable[..., tf.Tensor],
    ) -> tf.Tensor:
        raise VehicleBuiltinUnsupported("OptimiseRatTensor")

    @override
    def If(self, cond: vcl.Bool, ifTrue: _T, ifFalse: _T) -> _T:
        return cast(_T, tf.cond(cond, lambda: ifTrue, lambda: ifFalse))

    @override
    def DimensionLookup(self, xs: tf.Tensor, i: vcl.Index) -> vcl.Nat:
        # Get the shape dimension and convert to float for arithmetic compatibility
        shape_tensor = tf.shape(xs)
        dim_value = tf.gather(shape_tensor, i)
        return cast(vcl.Nat, tf.cast(dim_value, self.dtype_rat))

    @override
    def DimensionCons(
        self, head: vcl.Nat, tail: Tuple[vcl.Nat, ...]
    ) -> Tuple[vcl.Nat, ...]:
        return (head, *tail)

    @override
    def DimensionNil(self) -> Tuple[vcl.Nat, ...]:
        return ()

    @override
    def ConstTensor(self, value: vcl.Rat, shape: Tuple[vcl.Nat, ...]) -> tf.Tensor:
        return _tf_constant(value=float(value), shape=shape, dtype=self.dtype_rat)

    @override
    def DenseTensor(
        self, values: Tuple[vcl.Rat, ...], shape: Tuple[vcl.Nat, ...]
    ) -> tf.Tensor:
        # Convert Fraction values to floats and reshape to the specified shape
        float_values = [float(val) for val in values]
        return _tf_constant(value=float_values, shape=shape, dtype=self.dtype_rat)


@dataclass(frozen=True, init=False)
class TensorFlowTranslation(PythonTranslation):
    def __init__(self) -> None:
        super().__init__(
            builtins=TensorFlowBuiltins(),
            module_header=[
                py.Import(
                    names=[
                        py.alias(
                            name="tensorflow",
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


def load(
    path: str | Path,
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = Explicit.Explicit,
    translation: Optional[TensorFlowTranslation] = None,
) -> Dict[str, Any]:
    if translation is None:
        translation = TensorFlowTranslation()
    return translation.compile(
        ast_load(path, declarations=declarations, target=target),
        path=path,
    )


def load_loss_function(
    path: str | Path,
    property_name: DeclarationName,
    *,
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
    optimisers: AnyOptimisers = {},
) -> Callable[..., tf.Tensor]:
    """
    Load a loss function from a property in a Vehicle specification.

    :param path: The path to the Vehicle specification file.
    :param property_name: The name of the Vehicle property to load.
    :param target: The differentiable logic to use for interpreting the Vehicle property as a loss function, defaults to the Vehicle logic.
    :param samplers: A map from quantified variable names to samplers for their values. See `Sampler` for more details.
    :return: A function that takes the required external resources in the specification as keyword arguments and returns the loss corresponding to the property.
    """
    declarations = load(path, declarations=(property_name,), target=target)
    if property_name in declarations:
        property_func = declarations[property_name]
        if callable(property_func):
            return cast(Callable[..., tf.Tensor], property_func)
        else:
            raise VehiclePropertyNotCallable(property_name)
    else:
        raise VehiclePropertyNotFound(property_name)
