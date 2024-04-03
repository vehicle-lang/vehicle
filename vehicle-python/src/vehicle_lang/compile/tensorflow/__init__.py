from dataclasses import dataclass
from fractions import Fraction
from io import UnsupportedOperation
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, List, Optional, Type, Union

import tensorflow as tf  # type: ignore
from typing_extensions import Literal, TypeVar, override

from ... import ast as vcl
from ...typing import (
    AnyOptimisers,
    DeclarationName,
    DifferentiableLogic,
    Explicit,
    Target,
)
from ..abc import ABCBuiltins
from ..error import VehicleBuiltinUnsupported, VehiclePropertyNotFound
from ..python import PythonTranslation
from . import types as tfty

__all__: List[str] = [
    # Abstract Syntax Tree
    "AST",
    "Binder",
    "BuiltinFunction",
    "Declaration",
    "Expression",
    "Program",
    "Provenance",
    # Translation to Python
    "PythonBuiltins",
    "PythonTranslation",
    # High-level functions
    "compile",
    "load_loss_function",
]

################################################################################
### Interpretations of Vehicle builtins in Tensorflow
################################################################################


_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")


@dataclass(frozen=True)
class TensorFlowBuiltins(
    ABCBuiltins[
        tfty.Index,
        tfty.Bool,
        tfty.Nat,
        tfty.Int,
        tfty.Rat,
        tfty.IndexTensor,
        tfty.BoolTensor,
        tfty.NatTensor,
        tfty.IntTensor,
        tfty.RatTensor,
    ]
):
    dtype_index: tf.DType = tf.uint32
    dtype_bool: tf.DType = tf.bool
    dtype_nat: tf.DType = tf.uint32
    dtype_int: tf.DType = tf.int32
    dtype_rat: tf.DType = tf.float32

    @override
    def IndexType(self) -> Type[tfty.Index]:
        return tfty.Index

    @override
    def BoolTensorType(self) -> Type[tfty.BoolTensor]:
        return tfty.BoolTensor

    @override
    def IndexTensorType(self) -> Type[tfty.IndexTensor]:
        return tfty.IndexTensor

    @override
    def NatTensorType(self) -> Type[tfty.NatTensor]:
        return tfty.NatTensor

    @override
    def IntTensorType(self) -> Type[tfty.IntTensor]:
        return tfty.IntTensor

    @override
    def RatTensorType(self) -> Type[tfty.RatTensor]:
        return tfty.RatTensor

    @override
    def BoolTensor(self, value: vcl.Tensor[bool]) -> tfty.BoolTensor:
        return tf.constant(value=value.value, shape=value.shape, dtype=self.dtype_bool)

    @override
    def NatTensor(self, value: vcl.Tensor[int]) -> tfty.NatTensor:
        return tf.constant(value=value.value, shape=value.shape, dtype=self.dtype_nat)

    @override
    def IntTensor(self, value: vcl.Tensor[int]) -> tfty.IntTensor:
        return tf.constant(value=value.value, shape=value.shape, dtype=self.dtype_int)

    @override
    def RatTensor(self, value: vcl.Tensor[Fraction]) -> tfty.RatTensor:
        return tf.constant(
            value=(f.__float__() for f in value.value),
            shape=value.shape,
            dtype=self.dtype_rat,
        )

    @override
    def NotBoolTensor(self, x: tfty.BoolTensor) -> tfty.BoolTensor:
        return not x

    @override
    def AndBoolTensor(self, x: tfty.BoolTensor, y: tfty.BoolTensor) -> tfty.BoolTensor:
        return x and y

    @override
    def OrBoolTensor(self, x: tfty.BoolTensor, y: tfty.BoolTensor) -> tfty.BoolTensor:
        return x or y

    @override
    def NegRatTensor(self, x: tfty.RatTensor) -> tfty.RatTensor:
        return tf.negative(x)

    @override
    def AddRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.RatTensor:
        return tf.add(x, y)

    @override
    def SubRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.RatTensor:
        return tf.subtract(x, y)

    @override
    def MulRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.RatTensor:
        return tf.multiply(x, y)

    @override
    def DivRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.RatTensor:
        return tf.divide(x, y)

    @override
    def EqRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.BoolTensor:
        return tf.equal(x, y)

    @override
    def NeRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.BoolTensor:
        return tf.not_equal(x, y)

    @override
    def LeRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.BoolTensor:
        return tf.less_equal(x, y)

    @override
    def LtRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.BoolTensor:
        return tf.less(x, y)

    @override
    def GeRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.BoolTensor:
        return tf.greater_equal(x, y)

    @override
    def GtRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.BoolTensor:
        return tf.greater(x, y)

    @override
    def PowRatTensor(self, x: tfty.RatTensor, y: tfty.RatTensor) -> tfty.RatTensor:
        return tf.pow(x, y)

    @override
    def MinRatTensor(self, x: tfty.RatTensor) -> tfty.RatTensor:
        return tf.reduce_min(x)

    @override
    def MaxRatTensor(self, x: tfty.RatTensor) -> tfty.RatTensor:
        return tf.reduce_max(x)

    @override
    def ReduceAndBoolTensor(self, x: tfty.BoolTensor) -> tfty.BoolTensor:
        return tf.reduce_all(x)

    @override
    def ReduceOrBoolTensor(self, x: tfty.BoolTensor) -> tfty.BoolTensor:
        return tf.reduce_any(x)

    @override
    def ReduceSumRatTensor(self, x: tfty.RatTensor) -> tfty.RatTensor:
        return tf.reduce_sum(x)

    @override
    def ReduceRatTensor(
        self,
        f: Callable[[tfty.RatTensor, tfty.RatTensor], tfty.RatTensor],
        x: tfty.RatTensor,
    ) -> tfty.RatTensor:
        return tf.foldr(f, x)

    @override
    def EqIndex(self, x: tfty.Index, y: tfty.Index) -> tfty.Bool:
        return x == y

    @override
    def NeIndex(self, x: tfty.Index, y: tfty.Index) -> tfty.Bool:
        return x != y

    @override
    def LeIndex(self, x: tfty.Index, y: tfty.Index) -> tfty.Bool:
        return x <= y

    @override
    def LtIndex(self, x: tfty.Index, y: tfty.Index) -> tfty.Bool:
        return x < y

    @override
    def GeIndex(self, x: tfty.Index, y: tfty.Index) -> tfty.Bool:
        return x >= y

    @override
    def GtIndex(self, x: tfty.Index, y: tfty.Index) -> tfty.Bool:
        return x > y

    @override
    def LookupRatTensor(self, x: tfty.RatTensor, i: tfty.IndexTensor) -> tfty.Rat:
        return x[i]

    @override
    def StackRatTensor(self, n: int, *xs: tfty.RatTensor) -> tfty.RatTensor:
        return tf.stack(values=xs)

    @override
    def ConstRatTensor(self, value: tfty.Rat) -> tfty.RatTensor:
        return tf.repeat(value=value, dtype=self.dtype_rat)

    @override
    def MapRatTensor(
        self, f: Callable[[tfty.Rat], tfty.Rat], x: tfty.RatTensor
    ) -> tfty.RatTensor:
        return tf.map_fn(f, x, dtype=self.dtype_rat)

    @override
    def ZipWithRatTensor(
        self,
        f: Callable[[tfty.Rat, tfty.Rat], tfty.Rat],
        x: tfty.RatTensor,
        y: tfty.RatTensor,
    ) -> tfty.RatTensor:
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
    def IndicesIndexTensor(self, x: tfty.NatTensor) -> tfty.IndexTensor:
        raise VehicleBuiltinUnsupported("IndicesIndexTensor")

    @override
    def OptimiseRatTensor(
        self,
        minimiseOrMaximise: Literal["Minimise", "Maximise"],
        meetOrJoin: Callable[[tfty.RatTensor, tfty.RatTensor], tfty.RatTensor],
        loss: Callable[[tfty.Value], tfty.RatTensor],
    ) -> tfty.RatTensor:
        raise VehicleBuiltinUnsupported("OptimiseRatTensor")

    @override
    def If(
        self, cond: tfty.Bool, ifTrue: tfty.Value, ifFalse: tfty.Value
    ) -> tfty.Value:
        return tf.cond(cond, ifTrue, ifFalse)


@dataclass(frozen=True)
class TensorFlowTranslation(PythonTranslation):
    pass


def load(
    path: Union[str, Path],
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = Explicit.Explicit,
    translation: Optional[TensorFlowTranslation] = None,
) -> Dict[str, Any]:
    if translation is None:
        translation = TensorFlowTranslation(builtins=TensorFlowBuiltins(optimisers={}))
    return translation.compile(
        vcl.load(path, declarations=declarations, target=target), path=path
    )


def load_loss_function(
    path: Union[str, Path],
    property_name: DeclarationName,
    *,
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
    optimisers: AnyOptimisers = {},
) -> Any:
    """
    Load a loss function from a property in a Vehicle specification.

    :param path: The path to the Vehicle specification file.
    :param property_name: The name of the Vehicle property to load.
    :param target: The differentiable logic to use for interpreting the Vehicle property as a loss function, defaults to the Vehicle logic.
    :param samplers: A map from quantified variable names to samplers for their values. See `Sampler` for more details.
    :return: A function that takes the required external resources in the specification as keyword arguments and returns the loss corresponding to the property.
    """
    translation = TensorFlowTranslation(
        builtins=TensorFlowBuiltins(optimisers=optimisers)
    )
    declarations = load(
        path,
        declarations=(property_name,),
        target=target,
        translation=translation,
    )
    if property_name in declarations:
        return declarations[property_name]
    else:
        raise VehiclePropertyNotFound(property_name)
