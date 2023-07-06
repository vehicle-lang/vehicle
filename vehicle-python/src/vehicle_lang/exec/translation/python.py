import ast as py
from dataclasses import asdict, dataclass
from functools import partial, reduce
from typing import Any, ClassVar, Dict, List, Sequence

from typing_extensions import override

from .. import _ast as vcl
from ..builtin import Builtins
from . import ABCTranslation
from ._ast_compat import arguments as py_arguments
from ._ast_compat import dump as py_ast_dump
from ._ast_compat import unparse as py_ast_unparse


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


def py_app(func: py.expr, arg: py.arg, provenance: vcl.Provenance) -> py.expr:
    """Make a function call."""
    return py.Call(
        func=func,
        args=[arg],
        keywords=[],
        **asdict(provenance),
    )


def py_builtin(
    builtin: str, keywords: Sequence[py.keyword], provenance: vcl.Provenance
) -> py.expr:
    """Make a builtin function call."""
    return py.Call(
        func=py.Attribute(
            value=py.Name(
                id="__vehicle__",
                ctx=py.Load(),
                **asdict(provenance),
            ),
            attr=builtin,
            ctx=py.Load(),
            **asdict(provenance),
        ),
        args=[],
        keywords=list(keywords),
        **asdict(provenance),
    )


@dataclass(frozen=True)
class PythonTranslation(ABCTranslation[py.Module, py.stmt, py.expr]):
    builtins: Builtins[Any, Any, Any, Any, Any]
    module_header: ClassVar[Sequence[py.stmt]] = []
    module_footer: ClassVar[Sequence[py.stmt]] = []

    def compile(
        self,
        program: vcl.Program,
        filename: str,
        declaration_context: Dict[str, Any] = {},
    ) -> Dict[str, Any]:
        py_ast = self.translate_program(program)
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
            raise TypeError(f"{e}:\n{py_ast_str}")

    def translate_Main(self, program: vcl.Main) -> py.Module:
        return py.Module(
            body=[
                *self.__class__.module_header,
                *map(self.translate_declaration, program.declarations),
                *self.__class__.module_footer,
            ],
            type_ignores=[],
        )

    def translate_binder(self, binder: vcl.Binder) -> py.arguments:
        return py_binder(
            py.arg(
                arg=binder.name or "_",
                annotation=None,
                **asdict(binder.provenance),
            )
        )

    def translate_DefPostulate(self, declaration: vcl.DefPostulate) -> py.stmt:
        return py.Assert(
            test=py.Compare(
                left=py.Str(
                    s=declaration.name,
                    **asdict(declaration.provenance),
                ),
                ops=[py.In()],
                comparators=[
                    py.Call(
                        func=py.Name(
                            id="vars",
                            ctx=py.Load(),
                            **asdict(declaration.provenance),
                        ),
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

    def translate_DefFunction(self, declaration: vcl.DefFunction) -> py.stmt:
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

    def translate_App(self, expression: vcl.App) -> py.expr:
        # NOTE: Vector literals are handled separately, as Vehicle represents
        #       these as N-ary functions. These are intercepted here, at the
        #       App node, and passed directly to the Vector builtin.
        if (
            isinstance(expression.func, vcl.BuiltinOp)
            and isinstance(expression.func.builtin, vcl.Vector)
            and expression.func.builtin.value > 0
        ):
            return py_builtin(
                "Vector",
                [
                    py.keyword(
                        arg="values",
                        value=[
                            self.translate_expression(arg) for arg in expression.args
                        ],
                    )
                ],
                expression.provenance,
            )
        else:
            return reduce(
                partial(py_app, provenance=expression.provenance),
                [self.translate_expression(arg) for arg in expression.args],
                self.translate_expression(expression.func),
            )

    def translate_BoundVar(self, expression: vcl.BoundVar) -> py.expr:
        return py.Name(
            id=expression.name,
            ctx=py.Load(),
            **asdict(expression.provenance),
        )

    def translate_BuiltinOp(self, expression: vcl.BuiltinOp) -> py.expr:
        keywords: List[py.keyword] = []
        if isinstance(expression.builtin, vcl.Bool):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.NameConstant(
                        value=True,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Index):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Int):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Nat):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Rat):
            keywords.append(
                py.keyword(
                    arg="numerator",
                    value=py.Num(
                        n=expression.builtin.numerator,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
            keywords.append(
                py.keyword(
                    arg="denominator",
                    value=py.Num(
                        n=expression.builtin.denominator,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Vector):
            # NOTE: Vector literals are handled separately, as Vehicle represents
            #       these as N-ary functions. The only vectors that should be found
            #       at this point are empty vectors.
            assert (
                expression.builtin.value == 0
            ), "Found non-empty vector. These should be handled at their corresponding App node."
            keywords.append(
                py.keyword(
                    arg="values",
                    value=py.Num(
                        n=py.Tuple(elts=[], ctx=py.Load()),
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        return py_builtin(
            expression.builtin.__class__.__name__, keywords, expression.provenance
        )

    def translate_FreeVar(self, expression: vcl.FreeVar) -> py.expr:
        return py.Name(
            id=expression.name,
            ctx=py.Load(),
            **asdict(expression.provenance),
        )

    def translate_Lam(self, expression: vcl.Lam) -> py.expr:
        return py.Lambda(
            args=self.translate_binder(expression.binder),
            body=self.translate_expression(expression.body),
            **asdict(expression.provenance),
        )

    def translate_Pi(self, _expression: vcl.Pi) -> py.expr:
        return NotImplemented

    def translate_Universe(self, _expression: vcl.Universe) -> py.expr:
        return NotImplemented
