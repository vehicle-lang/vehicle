from dataclasses import dataclass
from typing import Any, Callable, Dict, Iterator

import tensorflow as tf
from typing_extensions import TypeAlias, override

from ._functools import Function2, Operator1, Operator2, curry
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
    def AddInt(self) -> Operator2[tf.Tensor]:
        return curry(tf.add)

    @override
    def AddNat(self) -> Operator2[tf.Tensor]:
        return curry(tf.add)

    @override
    def AddRat(self) -> Operator2[tf.Tensor]:
        return curry(tf.add)

    @override
    def DivRat(self) -> Operator2[tf.Tensor]:
        return curry(tf.divide)

    @override
    def MaxRat(self) -> Operator2[tf.Tensor]:
        return curry(tf.maximum)

    @override
    def MinRat(self) -> Operator2[tf.Tensor]:
        return curry(tf.minimum)

    @override
    def MulInt(self) -> Operator2[tf.Tensor]:
        return curry(tf.multiply)

    @override
    def MulNat(self) -> Operator2[tf.Tensor]:
        return curry(tf.multiply)

    @override
    def MulRat(self) -> Operator2[tf.Tensor]:
        return curry(tf.multiply)

    @override
    def NegInt(self) -> Operator1[tf.Tensor]:
        return lambda x: tf.negative(x)

    @override
    def NegRat(self) -> Operator1[tf.Tensor]:
        return lambda x: tf.negative(x)

    @override
    def PowRat(self) -> Function2[tf.Tensor, tf.Tensor, tf.Tensor]:
        return curry(tf.pow)

    @override
    def SubInt(self) -> Operator2[tf.Tensor]:
        return curry(tf.subtract)

    @override
    def SubRat(self) -> Operator2[tf.Tensor]:
        return curry(tf.subtract)
