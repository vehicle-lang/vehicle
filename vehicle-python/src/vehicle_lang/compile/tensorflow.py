from dataclasses import dataclass
from pathlib import Path
from typing import (
    Any,
    Dict,
    Iterable,
    List,
    Optional,
    SupportsFloat,
    SupportsInt,
    Union,
    cast,
)

import tensorflow as tf
from typing_extensions import override

from .. import ast as vcl
from ..typing import (
    AnyOptimisers,
    DeclarationName,
    DifferentiableLogic,
    Explicit,
    Target,
)
from .abc import Builtins
from .abcboolasbool import ABCBoolAsBoolBuiltins
from .error import VehiclePropertyNotFound
from .python import PythonTranslation

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


@dataclass(frozen=True)
class TensorflowBuiltins(ABCBoolAsBoolBuiltins[tf.Tensor, tf.Tensor, tf.Tensor]):
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
    def Int(self, value: SupportsInt) -> tf.Tensor:
        if tf.is_tensor(value):
            return cast(tf.Tensor, value)
        else:
            return tf.convert_to_tensor(value, dtype=self.dtypeInt)

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
    def Nat(self, value: SupportsInt) -> tf.Tensor:
        if tf.is_tensor(value):
            return cast(tf.Tensor, value)
        else:
            return tf.convert_to_tensor(value, dtype=self.dtypeNat)

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
    def Rat(self, value: SupportsFloat) -> tf.Tensor:
        if tf.is_tensor(value):
            return cast(tf.Tensor, value)
        else:
            return tf.convert_to_tensor(value, dtype=self.dtypeRat)

    @override
    def SubInt(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    @override
    def SubRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)


@dataclass(frozen=True)
class TensorflowTranslation(PythonTranslation):
    pass


def load(
    path: Union[str, Path],
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = Explicit.Explicit,
    translation: Optional[TensorflowTranslation] = None,
) -> Dict[str, Any]:
    if translation is None:
        translation = TensorflowTranslation(builtins=TensorflowBuiltins(optimisers={}))
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
    translation = TensorflowTranslation(
        builtins=TensorflowBuiltins(optimisers=optimisers)
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
