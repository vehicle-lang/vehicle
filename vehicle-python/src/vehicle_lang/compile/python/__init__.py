import ast as py
from dataclasses import asdict, dataclass, field
from fractions import Fraction
from functools import reduce
from pathlib import Path
from typing import Any, Dict, Iterator, List, Sequence, Tuple, Union

from ... import ast as vcl
from ..abc import ABCTranslation, AnyBuiltins
from ..error import VehicleOptimiseTypeError

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
        path: Union[str, Path],
        declaration_context: Dict[str, Any] = {},
    ) -> Dict[str, Any]:
        py_ast = self.translate_program(program)
        try:
            declaration_context["__vehicle__"] = self.builtins
            py_bytecode = compile(py_ast, filename=str(path), mode="exec")
            exec(py_bytecode, declaration_context)
            return declaration_context
        except TypeError as e:
            py_ast_str: str
            try:
                py_ast_str = py.unparse(py_ast)
            except:
                py_ast_str = py.dump(py_ast)
            raise TypeError(f"{e}\n{py_ast_str}")

    def translate_Main(self, program: vcl.Main) -> py.Module:
        return py.Module(
            body=[
                # NOTE: 'vehicle_lang.ast' is imported for 'Tensor'
                #       which is used to translate vcl.Tensor
                py.Import(
                    names=[py.alias(name="vehicle_lang.ast", **asdict(vcl.MISSING))],
                    level=0,
                    **asdict(vcl.MISSING),
                ),
                # NOTE: 'fractions' is imported for 'Fraction'
                #       which is used to translate vcl.Rat
                py.Import(
                    names=[py.alias(name="fractions", **asdict(vcl.MISSING))],
                    level=0,
                    **asdict(vcl.MISSING),
                ),
                # NOTE: 'functools' is imported for 'partial'
                #       which is used to translate vcl.PartialApp
                py.Import(
                    names=[py.alias(name="functools", **asdict(vcl.MISSING))],
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
        body = declaration.body
        binders = []
        while isinstance(body, vcl.Lam):
            binders.append(self.translate_binder(body.binder))
            body = body.body

        if binders:
            return py.FunctionDef(
                name=declaration.name,
                args=py_binder(*binders),
                body=[
                    py.Return(
                        value=self.translate_expression(body),
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
                    left=py.Constant(
                        value=declaration.name,
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
                msg=py.Constant(
                    value=f"The postulate {declaration.name} is undefined",
                    **asdict(declaration.provenance),
                ),
                **asdict(declaration.provenance),
            )

    def translate_App(self, expression: vcl.App) -> py.expr:
        # NOTE: We handle Minimise/Maximise as a special case, as we must
        #       extract the name of the bound variable from the lambda binding.
        if isinstance(expression.function, vcl.Builtin) and isinstance(
            expression.function.builtin, (vcl.MinimiseRatTensor, vcl.MaximiseRatTensor)
        ):
            if len(expression.arguments) != 2:
                raise VehicleOptimiseTypeError(expression)
            meetOrJoin, loss = expression.arguments
            if not isinstance(loss, vcl.Lam):
                raise VehicleOptimiseTypeError(expression)
            # NOTE: We extract the name of the bound variable from the lambda,
            #       which should be the _second_ argument.
            name = loss.binder.name
            return py_app(
                py_builtin(
                    builtin=expression.function.builtin.__class__.__name__,
                    provenance=expression.provenance,
                ),
                # name:
                py.Constant(
                    value=name,
                    **asdict(expression.provenance),
                ),
                # context:
                py.Dict(
                    keys=[
                        # py.Constant(
                        #     value=name,
                        #     **asdict(expression.provenance),
                        # )
                        # for name in expression.function.builtin.context
                    ],
                    values=[
                        # py_name(name, provenance=expression.provenance)
                        # for name in expression.function.builtin.context
                    ],
                    **asdict(expression.provenance),
                ),
                # meetOrJoin:
                self.translate_expression(meetOrJoin),
                # loss:
                self.translate_expression(loss),
                # provenance:
                provenance=expression.provenance,
            )
        return py_app(
            self.translate_expression(expression.function),
            *map(self.translate_expression, expression.arguments),
            provenance=expression.provenance,
        )

    def translate_Var(self, expression: vcl.Var) -> py.expr:
        return py_name(expression.name, provenance=expression.provenance)

    def translate_Builtin(self, expression: vcl.Builtin) -> py.expr:
        # MINIMISE/MAXIMISE
        #   All Minimise and Maximise nodes should be fully applied,
        #   and hence be captured by the translation for applications.
        if isinstance(
            expression.builtin, (vcl.MinimiseRatTensor, vcl.MaximiseRatTensor)
        ):
            raise VehicleOptimiseTypeError(expression)
        # TYPES
        #   When we encounter a type, we raise `EraseType`,
        #   which is handled by `translation_declarations`.
        elif isinstance(expression.builtin, vcl.BuiltinType):
            raise EraseType
        # CONSTANTS
        #   When we encounter a constant, we translate it to an application
        #   of the builtin function to the constant value, e.g., we translate
        #   `Index(value=3)` to `__vehicle__.Index(3)`.
        elif isinstance(
            expression.builtin,
            (vcl.BuiltinConstant, vcl.BuiltinLiteral),
        ):
            arguments: List[py.expr] = []
            if isinstance(expression.builtin, vcl.BuiltinLiteral):
                if isinstance(expression.builtin.value, Fraction):
                    arguments.append(
                        py_fraction(
                            expression.builtin.value,
                            provenance=expression.provenance,
                        )
                    )
                elif isinstance(expression.builtin.value, vcl.Tensor):
                    arguments.append(
                        py_tensor(
                            expression.builtin.value,
                            provenance=expression.provenance,
                        )
                    )
                else:
                    arguments.append(
                        py.Constant(
                            value=expression.builtin.value,
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
            assert isinstance(expression.builtin, vcl.BuiltinFunction)
            return py_builtin(
                builtin=expression.builtin.__class__.__name__,
                provenance=expression.provenance,
            )

    def translate_Lam(self, expression: vcl.Lam) -> py.expr:
        return py.Lambda(
            args=py_binder(self.translate_binder(expression.binder)),
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
    return py.arguments(
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


def py_fraction(value: Fraction, provenance: vcl.Provenance) -> py.expr:
    return py_app(
        py_qualified_name("fractions", "Fraction", provenance=provenance),
        py.Constant(
            value=value.numerator,
            **asdict(provenance),
        ),
        py.Constant(
            value=value.denominator,
            **asdict(provenance),
        ),
        provenance=provenance,
    )


def py_scalar(value: vcl.DType, provenance: vcl.Provenance) -> py.expr:
    """Make a scalar."""
    if isinstance(value, Fraction):
        return py_fraction(value, provenance=provenance)
    else:
        return py.Constant(
            value=value,
            **asdict(provenance),
        )


def py_tuple(elements: List[py.expr], provenance: vcl.Provenance) -> py.expr:
    """Make a tuple."""
    return py.Tuple(
        elts=list(elements),
        ctx=py.Load(),
        **asdict(provenance),
    )


def py_tensor(tensor: vcl.Tensor[vcl.DType], provenance: vcl.Provenance) -> py.expr:
    """Make a tensor."""
    return py.Call(
        func=py_qualified_name("vehicle_lang", "ast", "Tensor", provenance=provenance),
        args=[],
        keywords=[
            py.keyword(
                arg="value",
                value=py_tuple(
                    [py_scalar(elt, provenance=provenance) for elt in tensor.value],
                    provenance=provenance,
                ),
                **asdict(provenance),
            ),
            py.keyword(
                arg="shape",
                value=py_tuple(
                    [py_scalar(dim, provenance=provenance) for dim in tensor.shape],
                    provenance=provenance,
                ),
                **asdict(provenance),
            ),
        ],
        **asdict(provenance),
    )
