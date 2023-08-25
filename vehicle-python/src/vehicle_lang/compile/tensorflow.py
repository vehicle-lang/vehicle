from dataclasses import dataclass
from pathlib import Path
from typing import (
    Any,
    Callable,
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
from typing_extensions import TypeVar, override

from .. import ast as vcl
from ..typing import (
    AnyOptimisers,
    DeclarationName,
    DifferentiableLogic,
    Explicit,
    Target,
)
from ._collections import SupportsVector
from .abc import ABCBuiltins
from .error import VehicleBuiltinUnsupported, VehiclePropertyNotFound
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


_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")


@dataclass(frozen=True)
class TensorflowBuiltins(ABCBuiltins[tf.Tensor, tf.Tensor, tf.Tensor]):
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
    def AtVector(self, vector: SupportsVector[_T], index: int) -> _T:
        raise VehicleBuiltinUnsupported(vcl.AtVector.__name__)

    @override
    def DivRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.divide(x, y)

    @override
    def FoldVector(
        self, function: Callable[[_S, _T], _T], initial: _T, vector: SupportsVector[_S]
    ) -> _T:
        raise VehicleBuiltinUnsupported(vcl.FoldVector.__name__)

    @override
    def Int(self, value: SupportsInt) -> tf.Tensor:
        if tf.is_tensor(value):
            return cast(tf.Tensor, value)
        else:
            return tf.convert_to_tensor(value, dtype=self.dtypeInt)

    @override
    def MapVector(
        self, function: Callable[[_S], _T], vector: SupportsVector[_S]
    ) -> SupportsVector[_T]:
        raise VehicleBuiltinUnsupported(vcl.MapVector.__name__)

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

    @override
    def Vector(self, *values: _T) -> SupportsVector[_T]:
        raise VehicleBuiltinUnsupported(vcl.Vector.__name__)

    @override
    def ZipWithVector(
        self,
        function: Callable[[_S, _T], _U],
        vector1: SupportsVector[_S],
        vector2: SupportsVector[_T],
    ) -> SupportsVector[_U]:
        raise VehicleBuiltinUnsupported(vcl.ZipWithVector.__name__)


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
