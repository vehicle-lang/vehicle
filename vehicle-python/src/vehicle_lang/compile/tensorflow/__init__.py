import ast as py
from dataclasses import asdict, dataclass
from fractions import Fraction
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, List, Optional, Type, Union

import tensorflow as tf  # type: ignore
from typing_extensions import Literal, override

from ...ast import MISSING, Tensor
from ...ast import load as ast_load
from ...typing import (
    AnyOptimisers,
    DeclarationName,
    DifferentiableLogic,
    Explicit,
    Target,
)
from ..abc import ABCBuiltins, Value
from ..error import VehicleBuiltinUnsupported, VehiclePropertyNotFound
from ..python import PythonTranslation
from . import types as vcl

################################################################################
### Interpretations of Vehicle builtins in Tensorflow
################################################################################


@dataclass(frozen=True)
class TensorFlowBuiltins(
    ABCBuiltins[
        vcl.Index,
        vcl.Bool,
        vcl.Nat,
        vcl.Int,
        vcl.Rat,
        vcl.IndexTensor,
        vcl.BoolTensor,
        vcl.NatTensor,
        vcl.IntTensor,
        vcl.RatTensor,
    ]
):
    dtype_index: tf.DType = tf.uint32
    dtype_bool: tf.DType = tf.bool
    dtype_nat: tf.DType = tf.uint32
    dtype_int: tf.DType = tf.int32
    dtype_rat: tf.DType = tf.float32

    @override
    def IndexType(self) -> Type[vcl.Index]:
        return vcl.Index

    @override
    def BoolTensorType(self) -> Type[vcl.BoolTensor]:
        return vcl.BoolTensor

    @override
    def IndexTensorType(self) -> Type[vcl.IndexTensor]:
        return vcl.IndexTensor

    @override
    def NatTensorType(self) -> Type[vcl.NatTensor]:
        return vcl.NatTensor

    @override
    def IntTensorType(self) -> Type[vcl.IntTensor]:
        return vcl.IntTensor

    @override
    def RatTensorType(self) -> Type[vcl.RatTensor]:
        return vcl.RatTensor

    @override
    def BoolTensor(self, value: Tensor[bool]) -> vcl.BoolTensor:
        return tf.constant(value=value.value, dtype=self.dtype_bool, shape=value.shape)

    @override
    def NatTensor(self, value: Tensor[int]) -> vcl.NatTensor:
        return tf.constant(value=value.value, dtype=self.dtype_nat, shape=value.shape)

    @override
    def IntTensor(self, value: Tensor[int]) -> vcl.IntTensor:
        return tf.constant(value=value.value, dtype=self.dtype_int, shape=value.shape)

    @override
    def RatTensor(self, value: Tensor[Fraction]) -> vcl.RatTensor:
        return tf.constant(
            value=tuple(value.__float__() for value in value.value),
            dtype=self.dtype_rat,
            shape=value.shape,
        )

    @override
    def NotBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor:
        return not x

    @override
    def AndBoolTensor(self, x: vcl.BoolTensor, y: vcl.BoolTensor) -> vcl.BoolTensor:
        return x and y

    @override
    def OrBoolTensor(self, x: vcl.BoolTensor, y: vcl.BoolTensor) -> vcl.BoolTensor:
        return x or y

    @override
    def NegRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor:
        return tf.negative(x)

    @override
    def AddRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return tf.add(x, y)

    @override
    def SubRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return tf.subtract(x, y)

    @override
    def MulRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return tf.multiply(x, y)

    @override
    def DivRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return tf.divide(x, y)

    @override
    def EqRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return tf.equal(x, y)

    @override
    def NeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return tf.not_equal(x, y)

    @override
    def LeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return tf.less_equal(x, y)

    @override
    def LtRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return tf.less(x, y)

    @override
    def GeRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return tf.greater_equal(x, y)

    @override
    def GtRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.BoolTensor:
        return tf.greater(x, y)

    @override
    def PowRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return tf.pow(x, y)

    @override
    def MinRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return tf.minimum(x, y)

    @override
    def MaxRatTensor(self, x: vcl.RatTensor, y: vcl.RatTensor) -> vcl.RatTensor:
        return tf.maximum(x, y)

    @override
    def ReduceAndBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor:
        return tf.reduce_all(x)

    @override
    def ReduceOrBoolTensor(self, x: vcl.BoolTensor) -> vcl.BoolTensor:
        return tf.reduce_any(x)

    @override
    def ReduceSumRatTensor(self, x: vcl.RatTensor) -> vcl.RatTensor:
        return tf.reduce_sum(x)

    @override
    def ReduceRatTensor(
        self,
        f: Callable[[vcl.RatTensor, vcl.RatTensor], vcl.RatTensor],
        x: vcl.RatTensor,
    ) -> vcl.RatTensor:
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
    def LookupRatTensor(self, x: vcl.RatTensor, i: vcl.IndexTensor) -> vcl.Rat:
        return x[i]

    @override
    def StackRatTensor(self, n: int, *xs: vcl.RatTensor) -> vcl.RatTensor:
        return tf.stack(values=xs)

    @override
    def ConstRatTensor(self, value: vcl.Rat) -> vcl.RatTensor:
        return tf.repeat(value=value, dtype=self.dtype_rat)

    @override
    def MapRatTensor(
        self, f: Callable[[vcl.Rat], vcl.Rat], x: vcl.RatTensor
    ) -> vcl.RatTensor:
        return tf.map_fn(f, x, dtype=self.dtype_rat)

    @override
    def ZipWithRatTensor(
        self,
        f: Callable[[vcl.Rat, vcl.Rat], vcl.Rat],
        x: vcl.RatTensor,
        y: vcl.RatTensor,
    ) -> vcl.RatTensor:
        return tf.map_fn(
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
    def IndicesIndexTensor(self, x: vcl.NatTensor) -> vcl.IndexTensor:
        return x

    @override
    def OptimiseRatTensor(
        self,
        minimiseOrMaximise: Literal["Minimise", "Maximise"],
        meetOrJoin: Callable[[vcl.RatTensor, vcl.RatTensor], vcl.RatTensor],
        loss: Callable[[Value], vcl.RatTensor],
    ) -> vcl.RatTensor:
        raise VehicleBuiltinUnsupported("OptimiseRatTensor")

    @override
    def If(self, cond: vcl.Bool, ifTrue: Value, ifFalse: Value) -> Value:
        return tf.cond(cond, lambda: ifTrue, lambda: ifFalse)


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
                            **asdict(MISSING),
                        )
                    ],
                    **asdict(MISSING),
                )
            ],
        )


def load(
    path: Union[str, Path],
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
    path: Union[str, Path],
    property_name: DeclarationName,
    *,
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
    optimisers: AnyOptimisers = {},
) -> Callable[..., vcl.RatTensor]:
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
        return declarations[property_name]
    else:
        raise VehiclePropertyNotFound(property_name)
