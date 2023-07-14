from dataclasses import dataclass
from typing import Any, Callable, Dict, Iterator

import tensorflow as tf
from typing_extensions import TypeAlias, override

from .abc import Builtins

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

Sampler: TypeAlias = Callable[[Dict[str, Any]], Iterator[Any]]


@dataclass(frozen=True)
class TensorflowBuiltins(
    Builtins[
        tf.Tensor,
        tf.Tensor,
        tf.Tensor,
        tf.Tensor,
    ]
):
    dtypeNat: tf.DType = tf.uint64
    dtypeInt: tf.DType = tf.int64
    dtypeRat: tf.DType = tf.float64

    @override
    def AddInt(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.add(x, y)

    @override
    def AddNat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.add(x, y)

    @override
    def AddRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.add(x, y)

    @override
    def DivRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.divide(x, y)

    @override
    def MaxRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.maximum(x, y)

    @override
    def MinRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.minimum(x, y)

    @override
    def MulInt(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.multiply(x, y)

    @override
    def MulNat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.multiply(x, y)

    @override
    def MulRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.multiply(x, y)

    @override
    def NegInt(self, x: tf.Tensor) -> tf.Tensor:
        return tf.negative(x)

    @override
    def NegRat(self, x: tf.Tensor) -> tf.Tensor:
        return tf.negative(x)

    @override
    def PowRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.pow(x, y)

    @override
    def SubInt(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    @override
    def SubRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)
