import ast as py
from dataclasses import dataclass
from functools import reduce
from pathlib import Path
from typing import (
    Any,
    Callable,
    Dict,
    Iterable,
    List,
    Optional,
    Sequence,
    SupportsFloat,
    SupportsInt,
    Union,
    cast,
)

import numpy as np
import tensorflow as tf
from typing_extensions import TypeVar, override

from .. import ast as vcl
from ..typing import (
    AbstractVariableDomain,
    AnyDomains,
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
## Notion of a variable domain in tensorflow
################################################################################


class VariableDomain(AbstractVariableDomain[tf.Tensor]):
    @staticmethod
    def from_bounds(lower_bound: Any, upper_bound: Any) -> "VariableDomain":
        return VariableDomain(lower_bound, upper_bound)

    _lower_bounds: np.ndarray
    _upper_bounds: np.ndarray
    _shape: tf.TensorShape

    def __init__(self, lower_bounds: np.ndarray, upper_bounds: np.ndarray) -> None:
        self._lower_bounds = np.array(lower_bounds)
        self._upper_bounds = np.array(upper_bounds)

        lower_shape = self._lower_bounds.shape
        upper_shape = self._upper_bounds.shape
        if lower_shape != upper_shape:
            raise ValueError(
                f"Variable domain lower and upper bounds must be the same dimensions but found {lower_shape} vs {upper_shape}"
            )
        else:
            self._shape = tf.TensorShape(lower_shape)

    @override
    def dimensions(self) -> tf.TensorShape:
        return self._shape

    @override
    def random_value(self) -> tf.Tensor:
        return (self._lower_bounds + self._upper_bounds) / 2.0

    @override
    def clip(self, point: tf.Tensor) -> tf.Tensor:
        return tf.clip_by_value(point, self._lower_bounds, self._upper_bounds)


################################################################################
### Interpretations of Vehicle builtins in Tensorflow
################################################################################


_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")


@dataclass(frozen=True)
class TensorflowBuiltins(ABCBuiltins[tf.Tensor, tf.Tensor, tf.Tensor, tf.Variable]):
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
    def ConsVector(self, item: _T, vector: SupportsVector[_T]) -> SupportsVector[_T]:
        raise VehicleBuiltinUnsupported(vcl.ConsVector.__name__)

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
        return value.__int__()

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
        return value.__int__()

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
            return tf.convert_to_tensor(float(value), dtype=self.rat_dtype)

    @override
    def SubInt(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    @override
    def SubRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

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

    @override
    def OptimiseDefault(
        self,
        variable: tf.Variable,
        domain: AbstractVariableDomain[tf.Tensor],
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
    def create_quantified_variable(
        self, name: str, shape: Sequence[int]
    ) -> tf.Variable:
        initial_value = tf.constant(
            value=0, dtype=self.rat_dtype, shape=shape, name=f"{name}-initialValue"
        )

        return tf.Variable(
            initial_value=initial_value,
            name=name,
            trainable=True,
            dtype=self.rat_dtype,
        )


@dataclass(frozen=True)
class TensorflowTranslation(PythonTranslation):
    pass


def pgd(
    variable: tf.Variable,
    domain: AbstractVariableDomain[tf.Tensor],
    predicate: Callable[[tf.Tensor], tf.Tensor],
    minimise: bool,
    number_of_iterations: int = 5,
    alpha: float = 0.05,
) -> tf.Tensor:
    # Generate the starting point of the PGD attack
    variable.assign(domain.random_value())

    # Calculate which direction we're stepping the gradient.
    direction = 1 if minimise else -1

    for _ in range(number_of_iterations):
        # Calculate the loss for the current candidate value
        with tf.GradientTape() as tape:
            tape.watch(variable)
            loss = predicate(variable)

        # Get the gradients of the loss w.r.t to the input variable.
        gradient = tape.gradient(loss, variable)
        # Get the sign of the gradients to create the perturbation
        signed_grad = tf.sign(gradient)
        # Update the value of the variable
        perturbed_value = variable + direction * alpha * signed_grad
        # Clip the variable so it lies within the valid bounds.
        variable.assign(domain.clip(perturbed_value))

    return loss


def load_loss_function(
    path: Union[str, Path],
    property_name: DeclarationName,
    quantified_variable_domains: AnyDomains,
    *,
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
    quantified_variable_optimisers: AnyOptimisers = {},
    rat_dtype: tf.DType = tf.float32,
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
            quantified_variable_domains=quantified_variable_domains,
            quantified_variable_optimisers=quantified_variable_optimisers,
            rat_dtype=rat_dtype,
        )
    )

    uncompiled_program = vcl.load(
        path=path, declarations=(property_name,), target=target
    )

    program = translation.compile(uncompiled_program, path=path)

    if property_name in program:
        return program[property_name]
    else:
        raise VehiclePropertyNotFound(property_name)
