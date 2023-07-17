import ast as py
from dataclasses import asdict, dataclass, field
from functools import reduce
from pathlib import Path
from typing import (
    Any,
    Dict,
    Iterator,
    List,
    Optional,
    Sequence,
    SupportsFloat,
    SupportsInt,
    Union,
)

from typing_extensions import final, overload, override

from .. import ast as vcl
from .. import session as global_session
from .._target import Target
from ..session import Session
from ._ast_compat import arguments as py_arguments
from ._ast_compat import dump as py_ast_dump
from ._ast_compat import unparse as py_ast_unparse
from .abc import ABCTranslation, AnyBuiltins, Sampler
from .abcboolasbool import ABCBoolAsBoolBuiltins

################################################################################
### Implementation of Vehicle builtins in Python
################################################################################


@final
@dataclass(frozen=True)
class PythonBuiltins(ABCBoolAsBoolBuiltins[int, int, float]):
    @override
    def Int(self, value: SupportsInt) -> int:
        return value.__int__()

    @override
    def Nat(self, value: SupportsInt) -> int:
        return value.__int__()

    @override
    def Rat(self, value: SupportsFloat) -> float:
        return value.__float__()


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

    def compile(
        self,
        program: vcl.Program,
        filename: str,
        declaration_context: Dict[str, Any] = {},
    ) -> Dict[str, Any]:
        py_ast = self.translate_program(program)
        print(py_ast_unparse(py_ast))
        try:
            declaration_context["__vehicle__"] = self.builtins
            py_bytecode = compile(py_ast, filename=filename, mode="exec")
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
                # logging.warning(f"ignored declaration {name}")
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
                vcl.Sample,
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
            elif isinstance(expression.builtin, vcl.Sample):
                arguments.append(
                    py.Str(
                        s=expression.builtin.name,
                        **asdict(expression.provenance),
                    )
                )
                arguments.append(
                    py.Dict(
                        keys=[
                            py.Str(
                                s=local,
                                **asdict(expression.provenance),
                            )
                            for local in expression.builtin.locals
                        ],
                        values=[
                            py_name(local, provenance=expression.provenance)
                            for local in expression.builtin.locals
                        ],
                        **asdict(expression.provenance),
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


################################################################################
### Compilation to Python
################################################################################


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: Dict[str, Sampler[Any]],
    builtins: None = None,
    translation: None = None,
) -> Dict[str, Any]:
    ...


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: None = None,
    builtins: AnyBuiltins,
    translation: None = None,
) -> Dict[str, Any]:
    ...


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: None = None,
    builtins: None = None,
    translation: PythonTranslation,
) -> Dict[str, Any]:
    ...


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: None = None,
    builtins: None = None,
    translation: None = None,
) -> Dict[str, Any]:
    ...


def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: Optional[Dict[str, Sampler[Any]]] = None,
    builtins: Optional[AnyBuiltins] = None,
    translation: Optional[PythonTranslation] = None,
) -> Dict[str, Any]:
    # Ensure that specification_path is a Path
    if isinstance(specification_path, str):
        specification_path = Path(specification_path)

    # Load the specification file
    if session is not None:
        program = session.load(specification_path, target=target)
    else:
        program = global_session.load(specification_path, target=target)

    # The user can provide one of samplers, builtins, or translation:
    if samplers is not None:
        assert (
            builtins is None and translation is None
        ), "Only one of 'samplers', 'builtins', or 'translation' may be specified."
        translation = PythonTranslation(builtins=PythonBuiltins(samplers=samplers))
    elif builtins is not None:
        assert (
            samplers is None and translation is None
        ), "Only one of 'samplers', 'builtins', or 'translation' may be specified."
        translation = PythonTranslation(builtins=builtins)
    elif translation is not None:
        assert (
            samplers is None and builtins is None
        ), "Only one of 'samplers', 'builtins', or 'translation' may be specified."
    else:
        translation = PythonTranslation(builtins=PythonBuiltins())

    # Translate the specification to a Python module:
    specification_filename = specification_path.name
    return translation.compile(program, specification_filename, context)
