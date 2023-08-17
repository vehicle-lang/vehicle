from dataclasses import dataclass
from functools import reduce
from pathlib import Path
from typing import (
    Any,
    Callable,
    Dict,
    List,
    Optional,
    Sequence,
    SupportsFloat,
    SupportsInt,
    Tuple,
    Union,
)

import tensorflow as tf
from typing_extensions import TypeVar, override

from .. import ast as vcl
from ..typing import (
    DeclarationName,
    DifferentiableLogic,
    Domains,
    Optimisers,
    QuantifiedVariableName,
    VariableDomain,
    VehicleVector,
)
from .abc import ABCBuiltins
from .error import VehicleBuiltinUnsupported, VehiclePropertyNotFound
from .python import PythonTranslation
from .tensorflowpgd import pgd

__all__: List[str] = [
    # Abstract Syntax Tree
    "AST",
    "Binder",
    "BuiltinFunction",
    "Declaration",
    "Expression",
    "Program",
    "Provenance",
    # Translation to Tensorflow
    "TensorflowBuiltins",
    "TensorflowTranslation",
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
class TensorflowBuiltins(ABCBuiltins[tf.Tensor, tf.Tensor, tf.Tensor, tf.Variable]):
    dtype_nat: tf.DType = tf.uint64
    dtype_int: tf.DType = tf.int64
    dtype_rat: tf.DType = tf.float64

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
    def AtVector(self, vector: VehicleVector[_T], index: int) -> _T:
        raise VehicleBuiltinUnsupported(vcl.AtVector.__name__)

    @override
    def ConsVector(self, item: _T, vector: VehicleVector[_T]) -> VehicleVector[_T]:
        raise VehicleBuiltinUnsupported(vcl.ConsVector.__name__)

    @override
    def DivRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.divide(x, y)

    @override
    def FoldVector(
        self, function: Callable[[_S, _T], _T], initial: _T, vector: VehicleVector[_S]
    ) -> _T:
        raise VehicleBuiltinUnsupported(vcl.FoldVector.__name__)

    @override
    def Int(self, value: SupportsInt) -> tf.Tensor:
        return tf.convert_to_tensor(value, dtype=self.dtype_int)

    @override
    def MapVector(
        self, function: Callable[[_S], _T], vector: VehicleVector[_S]
    ) -> VehicleVector[_T]:
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
        return tf.convert_to_tensor(value, dtype=self.dtype_nat)

    @override
    def NegInt(self, x: tf.Tensor) -> tf.Tensor:
        return tf.negative(x)

    @override
    def NegRat(self, x: tf.Tensor) -> tf.Tensor:
        return tf.negative(x)

    @override
    def OptimiseDefault(
        self,
        variable: tf.Variable,
        domain: VariableDomain[tf.Tensor],
        minimise: bool,
        context: Dict[str, Any],
        joiner: Callable[[Any, Any], Any],
        predicate: Callable[[Any], Any],
    ) -> Any:
        del context
        pgd_losses = [pgd(variable, domain, predicate, minimise) for _ in range(10)]
        worst_loss = reduce(joiner, pgd_losses)
        return worst_loss

    @override
    def PowRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.pow(x, y)

    @override
    def QuantifiedVariable(self, name: str, shape: Tuple[int, ...]) -> tf.Variable:
        initial_value = tf.constant(
            value=0, dtype=self.dtype_rat, shape=shape, name=f"{name}-initialValue"
        )

        return tf.Variable(
            initial_value=initial_value,
            name=name,
            trainable=True,
            dtype=self.dtype_rat,
        )

    @override
    def Rat(self, value: SupportsFloat) -> tf.Tensor:
        return tf.convert_to_tensor(value, dtype=self.dtype_rat)

    @override
    def SubInt(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    @override
    def SubRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    def Vector(self, *values: _T) -> VehicleVector[_T]:
        raise VehicleBuiltinUnsupported(vcl.Vector.__name__)

    @override
    def ZipWithVector(
        self,
        function: Callable[[_S, _T], _U],
        vector1: VehicleVector[_S],
        vector2: VehicleVector[_T],
    ) -> VehicleVector[_U]:
        raise VehicleBuiltinUnsupported(vcl.ZipWithVector.__name__)


@dataclass(frozen=True)
class TensorflowTranslation(PythonTranslation):
    pass


def load_loss_function(
    specification_path: Union[str, Path],
    property_name: DeclarationName,
    *,
    dtype_nat: Optional[Callable[[SupportsInt], tf.Tensor]] = None,
    dtype_int: Optional[Callable[[SupportsInt], tf.Tensor]] = None,
    dtype_rat: Optional[Callable[[SupportsFloat], tf.Tensor]] = None,
    quantified_variables: Dict[QuantifiedVariableName, tf.Variable] = {},
    quantified_variable_domains: Domains[Any, tf.Tensor] = {},
    quantified_variable_optimisers: Optimisers[
        QuantifiedVariableName, tf.Tensor, Any, tf.Tensor
    ] = {},
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
) -> Any:
    """
    Load a loss function from a property in a Vehicle specification.

    :param path: The path to the Vehicle specification file.
    :param property_name: The name of the Vehicle property to load.
    :param quantified_variable_domains: A mapping from Vehicle functions to the domains of quantified variables.
    :param target: The differentiable logic to use for interpreting the Vehicle property as a loss function, defaults to the Vehicle logic.
    :param samplers: A map from quantified variable names to samplers for their values. See `Sampler` for more details.
    :param dtype: The tensorflow data type that the `Rat` type in Vehicle should be translated to, defaults to `float32`.
    :return: A function that takes the required external resources in the specification as keyword arguments and returns the loss corresponding to the property.
    """
    translation = TensorflowTranslation(
        builtins=TensorflowBuiltins(
            dtype_nat=dtype_nat or tf.uint64,
            dtype_int=dtype_int or tf.int64,
            dtype_rat=dtype_rat or tf.float64,
            quantified_variables=quantified_variables,
            quantified_variable_domains=quantified_variable_domains,
            quantified_variable_optimisers=quantified_variable_optimisers,
        )
    )
    program = translation.compile(
        vcl.load(
            specification_path,
            declarations=(property_name,),
            target=target,
        ),
        path=specification_path,
    )

    if property_name in program:
        return program[property_name]
    else:
        raise VehiclePropertyNotFound(property_name)
