from __future__ import annotations

from dataclasses import dataclass
from fractions import Fraction
from typing import TYPE_CHECKING, Any, Sequence, cast

from typing_extensions import override

from ..._deps import require_optional_dependency

if TYPE_CHECKING:
    import tensorflow as tf
else:  # pragma: no cover - exercised implicitly
    tf = require_optional_dependency(
        "tensorflow",
        extra="tensorflow",
        feature="The TensorFlow loss backend",
    )

from .. import error
from .._abc import ABCBuiltins
from .._ast import _nodes

################################################################################
### Type-safe TensorFlow wrappers
################################################################################


def _tf_constant(*args: Any, **kwargs: Any) -> tf.Tensor:
    """Type-safe wrapper for tf.constant that casts complex return type to tf.Tensor."""
    return cast(tf.Tensor, tf.constant(*args, **kwargs))


################################################################################
### Interpretations of Vehicle builtins in Tensorflow
################################################################################


@dataclass(frozen=True)
class TensorFlowBuiltins(
    ABCBuiltins[
        int,
        float,
        tf.Tensor,
    ]
):
    dtype_index: tf.DType = tf.uint32
    dtype_rat: tf.DType = tf.float32

    @override
    def RatTensor(self, value: _nodes.Tensor) -> tf.Tensor:
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
    def MinRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.minimum(x, y)

    @override
    def MaxRatTensor(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.maximum(x, y)

    @override
    def ReduceAddRatTensor(
        self, e: float, xs: tf.Tensor | Sequence[tf.Tensor]
    ) -> tf.Tensor:
        xs = tf.stack(xs)
        return tf.add(tf.reduce_sum(xs), e)

    @override
    def ReduceMulRatTensor(
        self, e: float, x: tf.Tensor | Sequence[tf.Tensor]
    ) -> tf.Tensor:
        x = tf.stack(x)
        return tf.multiply(tf.reduce_prod(x), e)

    @override
    def ReduceMinRatTensor(
        self, e: float, x: tf.Tensor | Sequence[tf.Tensor]
    ) -> tf.Tensor:
        x = tf.stack([tf.constant(e, dtype=self.dtype_rat)] + list(x))
        return tf.reduce_min(x)

    @override
    def ReduceMaxRatTensor(
        self, e: float, x: tf.Tensor | Sequence[tf.Tensor]
    ) -> tf.Tensor:
        x = tf.stack([tf.constant(e, dtype=self.dtype_rat)] + list(x))
        return tf.reduce_max(x)

    @override
    def DimensionLookup(
        self, xs: tf.Tensor | tuple[tf.Tensor, ...] | list[tf.Tensor], i: int
    ) -> tf.Tensor:
        # Despite the name, this implements element indexing (At operator in Haskell)
        # The JSON AST uses 'DimensionLookup' but semantics are element access

        # Handle tuple/sequence case (from StackTensor or similar)
        if isinstance(xs, (tuple, list)):
            return xs[i]

        if xs.shape.ndims == 0:
            raise error.VehicleInternalError(  # type: ignore[attr-defined]
                "Cannot index into a scalar tensor in DimensionLookup, make an issue in GitHub."
            )

        # Use tf.gather for proper type checking and TensorFlow best practices
        return tf.gather(xs, i)

    @override
    def DimensionCons(self, head: int, tail: Sequence[int]) -> tuple[int, ...]:
        return (head, *tail)

    @override
    def DimensionNil(self) -> tuple[int, ...]:
        return ()

    @override
    def ConstTensor(self, value: float, shape: Sequence[int]) -> tf.Tensor:
        return _tf_constant(value=float(value), shape=shape, dtype=self.dtype_rat)

    @override
    def DenseTensor(self, values: Sequence[float], shape: Sequence[int]) -> tf.Tensor:
        # Convert Fraction values to floats and reshape to the specified shape
        float_values = [float(val) for val in values]
        return _tf_constant(value=float_values, shape=shape, dtype=self.dtype_rat)

    @override
    def StackTensor(self, tensors: Sequence[tf.Tensor]) -> tf.Tensor:
        return tf.stack(tensors)
