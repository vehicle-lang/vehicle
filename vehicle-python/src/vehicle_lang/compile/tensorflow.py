import fractions
from dataclasses import dataclass
from functools import reduce
from pathlib import Path
from typing import (
    Any,
    Callable,
    Dict,
    List,
    Optional,
    SupportsFloat,
    SupportsInt,
    Tuple,
    Union,
    cast,
)

import tensorflow as tf
from typing_extensions import TypeVar, override

from .. import ast as vcl
from ..typing import (
    AnyContext,
    DeclarationName,
    DifferentiableLogic,
    Domains,
    Joiner,
    Minimise,
    Optimisers,
    Predicate,
    QuantifiedVariableName,
    VariableDomain,
    VehicleVector,
)
from .abc import ABCBuiltins
from .error import VehicleDomainNotFound, VehiclePropertyNotFound
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
    "BoundedVariableDomain",
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
class TensorflowBuiltins(ABCBuiltins[tf.Tensor, tf.Tensor, tf.Tensor]):
    dtype_nat: tf.DType = tf.uint64
    dtype_int: tf.DType = tf.int64
    dtype_rat: tf.DType = tf.float64

    domains: Domains[Any, tf.Tensor] = {}
    optimisers: Optimisers[tf.Variable, tf.Tensor, Any, tf.Tensor] = {}
    variables: Dict[QuantifiedVariableName, tf.Variable] = {}

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
        return vector[index]

    @override
    def ConsVector(self, item: _T, vector: VehicleVector[_T]) -> VehicleVector[_T]:
        # TODO: replace with efficient implementation
        return (item, *tuple(vector))

    @override
    def DivRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.divide(x, y)

    @override
    def FoldVector(
        self, function: Callable[[_S, _T], _T], initial: _T, vector: VehicleVector[_S]
    ) -> _T:
        # TODO: replace with efficient implementation
        return reduce(lambda x, y: function(y, x), tuple(vector), initial)

    @override
    def Int(self, value: SupportsInt) -> tf.Tensor:
        return tf.convert_to_tensor(value, dtype=self.dtype_int)

    @override
    def MapVector(
        self, function: Callable[[_S], _T], vector: VehicleVector[_S]
    ) -> VehicleVector[_T]:
        # TODO: replace with efficient implementation
        # return tf.map_fn(function, vector)
        return self.Vector(*map(function, tuple(vector)))

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
    def Optimise(
        self,
        name: QuantifiedVariableName,
        minimise: Minimise,
        context: AnyContext,
        joiner: Joiner[tf.Tensor],
        predicate: Predicate[tf.Tensor, tf.Tensor],
    ) -> tf.Tensor:
        # Ensure that a domain was provided
        if name not in self.domains:
            raise VehicleDomainNotFound(name)

        # Construct an instance of the domain from the context
        domain = self.domains[name](context)

        # Ensure that we have a variable
        if name not in self.variables:
            self.variables[name] = tf.Variable(
                initial_value=tf.constant(
                    value=0,
                    dtype=self.dtype_rat,
                    shape=domain.shape,
                    name=f"initial_value_for_{name}",
                ),
                name=name,
                trainable=True,
                dtype=self.dtype_rat,
            )
        variable = self.variables[name]

        # Ensure that we have an optimiser
        if name not in self.optimisers:
            self.optimisers[
                name
            ] = lambda variable, domain, minimise, _context, joiner, predicate: reduce(
                joiner, [pgd(variable, domain, predicate, minimise) for _ in range(10)]
            )
        optimiser = self.optimisers[name]

        # Run the optimiser
        return optimiser(
            variable,
            domain,
            minimise,
            context,
            joiner,
            predicate,
        )

    @override
    def PowRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.pow(x, y)

    @override
    def Rat(self, value: SupportsFloat) -> tf.Tensor:
        if isinstance(value, fractions.Fraction):
            value = value.__float__()
        return tf.convert_to_tensor(value, dtype=self.dtype_rat)

    @override
    def SubInt(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    @override
    def SubRat(self, x: tf.Tensor, y: tf.Tensor) -> tf.Tensor:
        return tf.subtract(x, y)

    def Vector(self, *values: _T) -> VehicleVector[_T]:
        return cast(VehicleVector[_T], tf.convert_to_tensor(values))

    @override
    def ZipWithVector(
        self,
        function: Callable[[_S, _T], _U],
        vector1: VehicleVector[_S],
        vector2: VehicleVector[_T],
    ) -> VehicleVector[_U]:
        # TODO: replace with efficient implementation
        # return tf.map_fn(function, tf.concat(vector1, vector2))
        return self.Vector(*map(function, tuple(vector1), tuple(vector2)))


################################################################################
### Bounded Variable Domain using TensorFlow
################################################################################


@dataclass(frozen=True)
class BoundedVariableDomain(VariableDomain[tf.Tensor]):
    lower_bounds: tf.Tensor
    upper_bounds: tf.Tensor
    dtype: tf.DType

    @staticmethod
    def from_bounds(
        lower_bounds: Any,
        upper_bounds: Any,
        *,
        dtype: tf.DType,
    ) -> VariableDomain[tf.Tensor]:
        return BoundedVariableDomain(
            lower_bounds=tf.convert_to_tensor(lower_bounds, dtype=dtype),
            upper_bounds=tf.convert_to_tensor(upper_bounds, dtype=dtype),
            dtype=dtype,
        )

    @override
    @property
    def shape(self) -> Tuple[int, ...]:
        return tuple(self.lower_bounds.shape.as_list())

    @override
    def random_value(self) -> tf.Tensor:
        return (self.lower_bounds + self.upper_bounds) / 2.0

    @override
    def clip(self, point: VehicleVector[tf.Tensor]) -> tf.Tensor:
        return tf.clip_by_value(
            tf.convert_to_tensor(point, dtype=self.dtype),
            self.lower_bounds,
            self.upper_bounds,
        )


################################################################################
### Translation from Vehicle AST to TensorFlow in Python AST
################################################################################


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
    domains: Domains[Any, tf.Tensor] = {},
    optimisers: Optimisers[tf.Variable, tf.Tensor, Any, tf.Tensor] = {},
    variables: Dict[QuantifiedVariableName, tf.Variable] = {},
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
            dtype_nat=dtype_nat or tf.uint32,
            dtype_int=dtype_int or tf.int32,
            dtype_rat=dtype_rat or tf.float32,
            domains=domains,
            optimisers=optimisers,
            variables=variables,
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
