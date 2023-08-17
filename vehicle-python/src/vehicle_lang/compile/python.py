import ast as py
import sys
from dataclasses import asdict, dataclass, field
from functools import reduce
from pathlib import Path
from typing import (
    TYPE_CHECKING,
    Any,
    Callable,
    Dict,
    Iterable,
    Iterator,
    List,
    Optional,
    Sequence,
    SupportsFloat,
    SupportsInt,
    Tuple,
    Type,
    Union,
)

import numpy as np
import numpy.typing as npt
from typing_extensions import Self, TypeVar, final, override

from vehicle_lang.typing import VariableDomain, VehicleVector

from .. import ast as vcl
from ..ast import (
    AST,
    Binder,
    BuiltinFunction,
    Declaration,
    Expression,
    Program,
    Provenance,
)
from ..typing import (
    AnyContext,
    DeclarationName,
    DifferentiableLogic,
    Domains,
    Explicit,
    Optimiser,
    Optimisers,
    QuantifiedVariableName,
    Target,
    VariableDomain,
    VehicleVector,
)
from ._ast_compat import arguments as py_arguments
from ._ast_compat import dump as py_ast_dump
from ._ast_compat import unparse as py_ast_unparse
from .abc import ABCBuiltins, ABCTranslation, AnyBuiltins
from .error import VehicleOptimiseTypeError, VehiclePropertyNotFound

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
    "PythonVariableDomain",
    # High-level functions
    "compile",
    "load_loss_function",
]


################################################################################
### Implementation of Vehicle builtins in Python
################################################################################

_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")

if TYPE_CHECKING or sys.version_info >= (3, 9):
    _Nat = TypeVar("_Nat", bound=np.unsignedinteger[Any])
    _Int = TypeVar("_Int", bound=np.signedinteger[Any])
    _Rat = TypeVar("_Rat", bound=np.floating[Any])
else:
    _Nat = TypeVar("_Nat", bound=np.unsignedinteger)
    _Int = TypeVar("_Int", bound=np.signedinteger)
    _Rat = TypeVar("_Rat", bound=np.floating)


@final
@dataclass(frozen=True)
class PythonBuiltins(ABCBuiltins[_Nat, _Int, _Rat, QuantifiedVariableName]):
    dtype_nat: Callable[[SupportsInt], _Nat]
    dtype_int: Callable[[SupportsInt], _Int]
    dtype_rat: Callable[[SupportsFloat], _Rat]

    @override
    def AtVector(self, vector: VehicleVector[_T], index: int) -> _T:
        assert isinstance(vector, VehicleVector), f"Expected vector, found {vector}"
        assert isinstance(index, int), f"Expected int, found {vector}"
        return vector[index]

    @override
    def ConsVector(self, item: _T, vector: VehicleVector[_T]) -> VehicleVector[_T]:
        return (item, *vector)

    @override
    def FoldVector(
        self, function: Callable[[_S, _T], _T], initial: _T, vector: VehicleVector[_S]
    ) -> _T:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(vector, VehicleVector), f"Expected vector, found {vector}"
        return reduce(lambda x, y: function(y, x), vector, initial)

    @override
    def Int(self, value: SupportsInt) -> _Int:
        return self.dtype_int(value)

    @override
    def MapVector(
        self, function: Callable[[_S], _T], vector: VehicleVector[_S]
    ) -> Tuple[_T, ...]:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(vector, VehicleVector), f"Expected vector, found {vector}"
        return tuple(map(function, vector))

    @override
    def Nat(self, value: SupportsInt) -> _Nat:
        return self.dtype_nat(value)

    @override
    def Rat(self, value: SupportsFloat) -> _Rat:
        return self.dtype_rat(value)

    def Vector(self, *values: _T) -> Tuple[_T, ...]:
        return values

    @override
    def ZipWithVector(
        self,
        function: Callable[[_S, _T], _U],
        vector1: VehicleVector[_S],
        vector2: VehicleVector[_T],
    ) -> Tuple[_U, ...]:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(vector1, VehicleVector), f"Expected vector, found {vector1}"
        assert isinstance(vector2, VehicleVector), f"Expected vector, found {vector2}"
        return tuple(map(function, vector1, vector2))

    @override
    def QuantifiedVariable(
        self, name: QuantifiedVariableName, shape: Sequence[int]
    ) -> QuantifiedVariableName:
        return name


################################################################################
### Bounded Variable Domain using Numpy
################################################################################

_DType = TypeVar("_DType", bound=np.generic)


@dataclass(frozen=True)
class PythonVariableDomain(VariableDomain[_DType]):
    lower_bounds: npt.NDArray[_DType]
    upper_bounds: npt.NDArray[_DType]
    dtype: Type[_DType]

    @staticmethod
    def from_bounds(
        lower_bounds: npt.ArrayLike,
        upper_bounds: npt.ArrayLike,
        *,
        dtype: Type[_DType],
    ) -> VariableDomain[_DType]:
        return PythonVariableDomain(
            lower_bounds=np.array(lower_bounds, dtype=dtype),
            upper_bounds=np.array(upper_bounds, dtype=dtype),
            dtype=dtype,
        )

    @override
    def dimensions(self) -> Tuple[int, ...]:
        return self.lower_bounds.shape

    @override
    def random_value(self) -> npt.NDArray[_DType]:
        return np.divide(np.add(self.lower_bounds, self.upper_bounds), 2.0)

    @override
    def clip(self, point: VehicleVector[_DType]) -> npt.NDArray[_DType]:
        return np.clip(
            np.array(point, dtype=self.dtype), self.lower_bounds, self.upper_bounds
        )


################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


class EraseType(Exception):
    pass


@dataclass(frozen=True)
class PythonTranslation(ABCTranslation[py.Module, py.stmt, py.expr]):
    builtins: AnyBuiltins
    module_header: Sequence[py.stmt] = field(default_factory=tuple)
    module_footer: Sequence[py.stmt] = field(default_factory=tuple)
    ignored_types: List[str] = field(init=False, default_factory=list)

    @classmethod
    def from_builtins(cls: Type[Self], builtins: AnyBuiltins) -> Self:
        return cls(builtins=builtins)

    @classmethod
    def from_optimisers(
        cls: Type[Self],
        dtype_nat: Callable[[SupportsInt], _Nat],
        dtype_int: Callable[[SupportsInt], _Int],
        dtype_rat: Callable[[SupportsFloat], _Rat],
        quantified_variable_domains: Dict[
            QuantifiedVariableName,
            Callable[[AnyContext], VariableDomain[Union[_Nat, _Int, _Rat]]],
        ],
        quantified_variable_optimisers: Dict[
            QuantifiedVariableName, Optimiser[str, Union[_Nat, _Int, _Rat], Any, _Rat]
        ],
    ) -> Self:
        return cls.from_builtins(
            builtins=PythonBuiltins(
                dtype_nat=dtype_nat,
                dtype_int=dtype_int,
                dtype_rat=dtype_rat,
                quantified_variables={},
                quantified_variable_domains=quantified_variable_domains,
                quantified_variable_optimisers=quantified_variable_optimisers,
            )
        )

    def compile(
        self,
        program: vcl.Program,
        path: Union[str, Path],
    ) -> Dict[str, Any]:
        py_ast = self.translate_program(program)
        try:
            declaration_context = {"__vehicle__": self.builtins}
            py_bytecode = compile(py_ast, filename=str(path), mode="exec")
            exec(py_bytecode, declaration_context)
            return declaration_context
        except TypeError as e:
            py_ast_str: str
            try:
                py_ast_str = py_ast_unparse(py_ast)
            except:
                py_ast_str = py_ast_dump(py_ast)
            raise TypeError(f"{e}\n{py_ast_str}")

    def translate_Main(self, program: vcl.Main) -> py.Module:
        return py.Module(
            body=[
                # NOTE: 'fractions' is imported for 'Fraction'
                #       which is used to translate vcl.Rat
                py.Import(
                    names=[
                        py.alias(
                            name="fractions", asname="fractions", **asdict(vcl.MISSING)
                        )
                    ],
                    level=0,
                    **asdict(vcl.MISSING),
                ),
                # NOTE: 'functools' is imported for 'partial'
                #       which is used to translate vcl.PartialApp
                py.Import(
                    names=[
                        py.alias(
                            name="functools", asname="functools", **asdict(vcl.MISSING)
                        )
                    ],
                    level=0,
                    **asdict(vcl.MISSING),
                ),
                *self.module_header,
                *self.translate_declarations(iter(program.declarations)),
                *self.module_footer,
            ],
            type_ignores=[],
        )

    def translate_binder(self, binder: vcl.Binder) -> py.arg:
        return py.arg(
            arg=binder.name or "_",
            annotation=None,
            **asdict(binder.provenance),
        )

    def translate_declarations(
        self, declarations: Iterator[vcl.Declaration]
    ) -> Iterator[py.stmt]:
        for declaration in declarations:
            try:
                yield self.translate_declaration(declaration)
            except EraseType:
                name = declaration.get_name()
                self.ignored_types.append(name)

    def translate_DefFunction(self, declaration: vcl.DefFunction) -> py.stmt:
        if isinstance(declaration.body, vcl.Lam):
            return py.FunctionDef(
                name=declaration.name,
                args=py_binder(
                    *(
                        self.translate_binder(binder)
                        for binder in declaration.body.binders
                    )
                ),
                body=[
                    py.Return(
                        value=self.translate_expression(declaration.body.body),
                        **asdict(declaration.provenance),
                    )
                ],
                decorator_list=[],
                **asdict(declaration.provenance),
            )
        else:
            return py.Assign(
                targets=[
                    py.Name(
                        id=declaration.name,
                        ctx=py.Store(),
                        **asdict(declaration.provenance),
                    )
                ],
                value=self.translate_expression(declaration.body),
                **asdict(declaration.provenance),
            )

    def translate_DefPostulate(self, declaration: vcl.DefPostulate) -> py.stmt:
        # NOTE: Postulates are compiled in one of two ways:
        #       (1) If the builtins object has a method of the same name, the
        #           postulate is compiled to the result of calling that method.
        if hasattr(self.builtins, declaration.name) and callable(
            getattr(self.builtins, declaration.name)
        ):
            return py.Assign(
                targets=[
                    py.Name(
                        id=declaration.name,
                        ctx=py.Store(),
                        **asdict(declaration.provenance),
                    )
                ],
                value=py_builtin(
                    builtin=declaration.name,
                    provenance=declaration.provenance,
                ),
                **asdict(declaration.provenance),
            )
        #       (2) Otherwise, the postulate is compiled to an assertion that
        #           checks that a function with that name is in the globals.
        else:
            return py.Assert(
                test=py.Compare(
                    left=py.Str(
                        s=declaration.name,
                        **asdict(declaration.provenance),
                    ),
                    ops=[py.In()],
                    comparators=[
                        py.Call(
                            func=py_name("vars", provenance=declaration.provenance),
                            args=[],
                            keywords=[],
                            **asdict(declaration.provenance),
                        )
                    ],
                    **asdict(declaration.provenance),
                ),
                msg=py.Str(
                    s=f"The postulate {declaration.name} is undefined",
                    **asdict(declaration.provenance),
                ),
                **asdict(declaration.provenance),
            )

    def translate_App(self, expression: vcl.App) -> py.expr:
        # NOTE: We handle Optimise as a special case, as we must extract the
        #       name of the bound variable from the lambda binding.
        if isinstance(expression.function, vcl.Builtin) and isinstance(
            expression.function.builtin, vcl.Optimise
        ):
            if len(expression.arguments) != 2:
                raise VehicleOptimiseTypeError(expression)
            joiner, predicate = expression.arguments
            if not isinstance(predicate, vcl.Lam):
                raise VehicleOptimiseTypeError(expression)
            if len(predicate.binders) != 1:
                raise VehicleOptimiseTypeError(expression)
            # NOTE: We extract the name of the bound variable from the lambda,
            #       which should be the _second_ argument.
            name = predicate.binders[0].name

            return py_app(
                py_builtin(
                    builtin=expression.function.builtin.__class__.__name__,
                    provenance=expression.provenance,
                ),
                # name:
                py.Str(
                    s=name,
                    **asdict(expression.provenance),
                ),
                # minimise:
                py.NameConstant(
                    value=expression.function.builtin.minimise,
                    **asdict(expression.provenance),
                ),
                # context:
                py.Dict(
                    keys=[
                        py.Str(
                            s=name,
                            **asdict(expression.provenance),
                        )
                        for name in expression.function.builtin.context
                    ],
                    values=[
                        py_name(name, provenance=expression.provenance)
                        for name in expression.function.builtin.context
                    ],
                    **asdict(expression.provenance),
                ),
                # joiner:
                self.translate_expression(joiner),
                # predicate:
                self.translate_expression(predicate),
                # provenance:
                provenance=expression.provenance,
            )
        return py_app(
            self.translate_expression(expression.function),
            *map(self.translate_expression, expression.arguments),
            provenance=expression.provenance,
        )

    def translate_BoundVar(self, expression: vcl.BoundVar) -> py.expr:
        return py_name(expression.name, provenance=expression.provenance)

    def translate_Builtin(self, expression: vcl.Builtin) -> py.expr:
        # TYPES
        #   When we encounter a type, we raise `EraseType`,
        #   which is handled by `translation_declarations`.
        if isinstance(
            expression.builtin,
            (
                vcl.BoolType,
                vcl.IndexType,
                vcl.IntType,
                vcl.ListType,
                vcl.NatType,
                vcl.RatType,
                vcl.VectorType,
                vcl.UnitType,
            ),
        ):
            raise EraseType
        # OPTIMISE
        #   All Optimise() nodes should be fully applied, and hence be captured
        #   by the translation for applications.
        elif isinstance(expression.builtin, vcl.Optimise):
            raise VehicleOptimiseTypeError(expression)
        # CONSTANTS
        #   When we encounter a constant, we translate it to an application
        #   of the builtin function to the constant value, e.g., we translate
        #   `Index(value=3)` to `__vehicle__.Index(3)`.
        #
        #   The `Vector` builtin is a special case. Vehicle represents the
        #   empty vector as `Builtin(Vector(0))` but represents the vector with
        #   some elements as `App(Builtin(Vector(n)), [...])`.
        #   If the length of the vector is zero, we translate the vector as a
        #   constant, but otherwise translate it as a function.
        elif isinstance(
            expression.builtin,
            (
                vcl.Bool,
                vcl.Index,
                vcl.Int,
                vcl.Nat,
                vcl.NilList,
                vcl.Rat,
            ),
        ) or (
            isinstance(expression.builtin, vcl.Vector) and expression.builtin.value == 0
        ):
            arguments: List[py.expr] = []
            if isinstance(expression.builtin, vcl.Bool):
                arguments.append(
                    py.NameConstant(
                        value=expression.builtin.value,
                        **asdict(expression.provenance),
                    )
                )
            elif isinstance(expression.builtin, vcl.Index):
                arguments.append(
                    py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    )
                )
            elif isinstance(expression.builtin, vcl.Int):
                arguments.append(
                    py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    )
                )
            elif isinstance(expression.builtin, vcl.Nat):
                arguments.append(
                    py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    )
                )
            elif isinstance(expression.builtin, vcl.Rat):
                arguments.append(
                    py_app(
                        py_qualified_name(
                            "fractions",
                            "Fraction",
                            provenance=expression.provenance,
                        ),
                        py.Num(
                            n=expression.builtin.numerator,
                            **asdict(expression.provenance),
                        ),
                        py.Num(
                            n=expression.builtin.denominator,
                            **asdict(expression.provenance),
                        ),
                        provenance=expression.provenance,
                    )
                )
            return py_app(
                py_builtin(
                    expression.builtin.__class__.__name__,
                    provenance=expression.provenance,
                ),
                *arguments,
                provenance=expression.provenance,
            )
        # FUNCTIONS
        #   When we encounter a function, we translate it to the unapplied
        #   function name, e.g., we translate `AddInt` as `__vehicle__.AddInt`.
        else:
            return py_builtin(
                builtin=expression.builtin.__class__.__name__,
                provenance=expression.provenance,
            )

    def translate_FreeVar(self, expression: vcl.FreeVar) -> py.expr:
        # NOTE: We ignore any declaration where translation touches a type.
        if expression.name in self.ignored_types:
            raise EraseType()
        else:
            return py_name(expression.name, provenance=expression.provenance)

    def translate_Lam(self, expression: vcl.Lam) -> py.expr:
        return py.Lambda(
            args=py_binder(*(map(self.translate_binder, expression.binders))),
            body=self.translate_expression(expression.body),
            **asdict(expression.provenance),
        )

    def translate_Pi(self, _expression: vcl.Pi) -> py.expr:
        raise EraseType()

    def translate_PartialApp(self, expression: vcl.PartialApp) -> py.expr:
        return py_partial_app(
            self.translate_expression(expression.function),
            *map(self.translate_expression, expression.arguments),
            provenance=expression.provenance,
        )

    def translate_Universe(self, _expression: vcl.Universe) -> py.expr:
        raise EraseType()


def py_name(name: vcl.Name, *, provenance: vcl.Provenance) -> py.Name:
    """Make a name."""
    return py.Name(
        id=name,
        ctx=py.Load(),
        **asdict(provenance),
    )


def py_qualified_name(*parts: vcl.Name, provenance: vcl.Provenance) -> py.expr:
    """Make a qualified name."""
    if not parts:
        raise ValueError("A qualified name should have at least one part.")

    def py_attribute(value: py.expr, attr: str) -> py.expr:
        return py.Attribute(value=value, attr=attr, ctx=py.Load(), **asdict(provenance))

    initial: py.expr = py_name(parts[0], provenance=provenance)
    return reduce(py_attribute, parts[1:], initial)


def py_binder(*args: py.arg) -> py.arguments:
    """Make a binder which only uses args."""
    return py_arguments(
        posonlyargs=[],
        args=list(args),
        vararg=None,
        kwonlyargs=[],
        kw_defaults=[],
        kwarg=None,
        defaults=[],
    )


def py_builtin(builtin: str, *, provenance: vcl.Provenance) -> py.expr:
    """Make a builtin function call."""
    return py_qualified_name("__vehicle__", builtin, provenance=provenance)


def py_app(
    function: py.expr, *arguments: py.expr, provenance: vcl.Provenance
) -> py.expr:
    """Make a function call."""
    return py.Call(
        func=function,
        args=list(arguments),
        keywords=[],
        **asdict(provenance),
    )


def py_partial_app(
    function: py.expr, *arguments: py.expr, provenance: vcl.Provenance
) -> py.expr:
    """Make a partial function call."""
    return py_app(
        py_qualified_name("functools", "partial", provenance=provenance),
        function,
        *arguments,
        provenance=provenance,
    )


def load(
    path: Union[str, Path],
    *,
    declarations: Iterable[DeclarationName] = (),
    dtype_nat: Optional[Callable[[SupportsInt], _Nat]] = None,
    dtype_int: Optional[Callable[[SupportsInt], _Int]] = None,
    dtype_rat: Optional[Callable[[SupportsFloat], _Rat]] = None,
    quantified_variable_domains: Domains[Any, Union[_Nat, _Int, _Rat]] = {},
    quantified_variable_optimisers: Optimisers[
        QuantifiedVariableName, Union[_Nat, _Int, _Rat], Any, _Rat
    ] = {},
    target: Target = Explicit.Explicit,
) -> Dict[str, Any]:
    translation = PythonTranslation(
        builtins=PythonBuiltins(
            dtype_nat=dtype_nat or np.uint32,
            dtype_int=dtype_int or np.int32,
            dtype_rat=dtype_rat or np.float32,
            quantified_variables={v: v for v in quantified_variable_domains.keys()},
            quantified_variable_domains=quantified_variable_domains,
            quantified_variable_optimisers=quantified_variable_optimisers,
        )
    )
    return translation.compile(
        vcl.load(path, declarations=declarations, target=target), path=path
    )


def load_loss_function(
    path: Union[str, Path],
    property_name: DeclarationName,
    *,
    dtype_nat: Optional[Callable[[SupportsInt], _Nat]] = None,
    dtype_int: Optional[Callable[[SupportsInt], _Int]] = None,
    dtype_rat: Optional[Callable[[SupportsFloat], _Rat]] = None,
    quantified_variable_domains: Domains[Any, Union[_Nat, _Int, _Rat]] = {},
    quantified_variable_optimisers: Optimisers[
        QuantifiedVariableName, Union[_Nat, _Int, _Rat], Any, _Rat
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
    :return: A function that takes the required external resources in the specification as keyword arguments and returns the loss corresponding to the property.
    """

    declarations = load(
        path,
        declarations=(property_name,),
        target=target,
        dtype_nat=dtype_nat,
        dtype_int=dtype_int,
        dtype_rat=dtype_rat,
        quantified_variable_domains=quantified_variable_domains,
        quantified_variable_optimisers=quantified_variable_optimisers,
    )
    if property_name in declarations:
        return declarations[property_name]
    else:
        raise VehiclePropertyNotFound(property_name)
