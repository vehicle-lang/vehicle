from __future__ import annotations

import collections.abc
from dataclasses import dataclass
from fractions import Fraction
from typing import TYPE_CHECKING, Any, Callable, List, Sequence, Tuple, cast

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

from ... import error
from ..._ast import _nodes
from .._abc import ABCBuiltins

################################################################################
### Type-safe TensorFlow wrappers
################################################################################


def _tf_constant(*args: Any, **kwargs: Any) -> tf.Tensor:
    """Type-safe wrapper for tf.constant that casts complex return type to tf.Tensor."""
    return cast(tf.Tensor, tf.constant(*args, **kwargs))


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


def _comparison(op: str, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
    match op:
        case "Eq":
            return tf.equal(x, y)
        case "Ne":
            return tf.not_equal(x, y)
        case "Le":
            return tf.less_equal(x, y)
        case "Lt":
            return tf.less(x, y)
        case "Ge":
            return tf.greater_equal(x, y)
        case "Gt":
            return tf.greater(x, y)
        case _:
            raise error.VehicleInternalError(f"Unknown comparison operation: {op}")


################################################################################
### Interpretations of Vehicle builtins in Tensorflow
################################################################################


@dataclass(frozen=True)
class TensorFlowBuiltins(
    ABCBuiltins[
        int,
        float,
        tf.Tensor,
        List[Any],
    ]
):
    dtype_index: tf.DType = tf.uint32
    dtype_rat: tf.DType = tf.float32

    @override
    def BoolTensor(self, x: _nodes.Tensor[bool]) -> tf.Tensor:
        match x:
            case _nodes.DenseTensor():
                values = x.values
            case _nodes.ConstantTensor():
                values = (x.value,)
            case _:
                raise error.VehicleInternalError(f"Unknown tensor type: {type(x)}.")

        return _tf_constant(data=values, dtype=tf.bool, shape=x.shape)

    @override
    def BoolNot(self, x: tf.Tensor) -> tf.Tensor:
        return tf.logical_not(x)

    @override
    def BoolAnd(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.logical_and(x, y)

    @override
    def BoolOr(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.logical_or(x, y)

    @override
    def BoolImplies(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.logical_or(tf.logical_not(x), y)

    @override
    def BoolCompareIndex(self, op: str, x: int, y: int) -> tf.Tensor:
        tx = _tf_constant(value=x, dtype=self.dtype_index)
        ty = _tf_constant(value=y, dtype=self.dtype_index)
        return _comparison(op, tx, ty)

    @override
    def BoolCompareNat(self, op: str, x: int, y: int) -> tf.Tensor:
        tx = _tf_constant(value=x, dtype=self.dtype_index)
        ty = _tf_constant(value=y, dtype=self.dtype_index)
        return _comparison(op, tx, ty)

    @override
    def BoolCompareRatTensor(
        self,
        op: str,
        pointwise_dims: Sequence[int],
        reduce_dims: Sequence[int],
        x: tf.Tensor,
        y: tf.Tensor,
    ) -> tf.Tensor:
        _ = pointwise_dims
        result = _comparison(op, x, y)
        for _ in reduce_dims:
            result = tf.reduce_all(result, axis=-1)
        return result

    @override
    def BoolReduceAnd(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_all(x)

    @override
    def BoolReduceOr(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_any(x)

    @override
    def BoolIf(self, c: tf.Tensor, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.where(c, x, y)

    @override
    def RatTensor(self, x: _nodes.Tensor[_nodes.ExtendedFraction]) -> tf.Tensor:
        match x:
            case _nodes.DenseTensor():
                float_values = tuple(
                    _extended_rational_to_float(val) for val in x.values
                )
            case _nodes.ConstantTensor():
                float_values = (_extended_rational_to_float(x.value),)
            case _:
                raise error.VehicleInternalError(
                    f"Unexpected type for RatTensor: {type(x)}"
                )

        return _tf_constant(value=float_values, dtype=self.dtype_rat, shape=x.shape)

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
    def PowRatTensor(self, x: tf.Tensor, y: float) -> tf.Tensor:
        return tf.pow(x, tf.fill(dims=x.shape, value=y, dtype=self.dtype_rat))

    @override
    def LogRatTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.math.log(x)

    @override
    def ExpRatTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.math.exp(x)

    @override
    def ReduceAddRatTensor(self, xs: tf.Tensor) -> tf.Tensor:
        return tf.reduce_sum(xs)

    @override
    def ReduceMulRatTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_prod(x)

    @override
    def ReduceMinRatTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_min(x)

    @override
    def ReduceMaxRatTensor(self, x: tf.Tensor) -> tf.Tensor:
        return tf.reduce_max(x)

    @override
    def WhereTensor(
        self, input: tf.Tensor, condition: tf.Tensor, false_value: tf.Tensor
    ) -> tf.Tensor:
        return tf.where(condition=condition, x=input, y=false_value)

    @override
    def Transpose(self, xs: tf.Tensor) -> tf.Tensor:
        return tf.transpose(xs)

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

    @override
    def AtTensor(
        self, xs: tf.Tensor | tuple[tf.Tensor, ...] | list[tf.Tensor], i: int
    ) -> tf.Tensor:
        # Handle tuple/sequence case (from StackTensor or similar)
        if isinstance(xs, (tuple, list)):
            return xs[i]

        if xs.shape.ndims == 0:
            raise error.VehicleInternalError(
                "Cannot index into a scalar tensor in AtTensor, make an issue in GitHub."
            )

        # Use tf.gather for proper type checking and TensorFlow best practices
        return tf.gather(xs, i)

    @override
    def ForeachTensor(
        self, size: int, function: Callable[[int], tf.Tensor]
    ) -> tf.Tensor:
        # Apply the function to each index and stack the results
        return tf.stack([function(i) for i in range(size)])

    @override
    def VectorLiteral(self, xs: Sequence[Any]) -> List[Any]:
        return list(xs)

    @override
    def AtVector(self, xs: tf.Tensor | Tuple[Any, ...] | List[Any], i: int) -> Any:
        if isinstance(xs, tf.Tensor):
            if xs.shape.ndims != 0:
                return tf.gather(xs, i)

            raise error.VehicleInternalError(
                "Cannot index into a scalar tensor in AtVector, make an issue in GitHub."
            )

        return xs[i]

    @override
    def ForeachVector(self, size: int, function: Callable[[int], Any]) -> List[Any]:
        # Apply the function to each index and stack the results
        return [function(i) for i in range(size)]
