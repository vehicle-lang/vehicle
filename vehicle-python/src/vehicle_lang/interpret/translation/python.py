import ast as py
from dataclasses import asdict, dataclass
from typing import Any, ClassVar, Dict, List, Optional, Sequence

from typing_extensions import override

from .. import _ast as vcl
from ..builtin import BuiltinInterpreter
from . import ABCTranslation
from ._ast_compat import arguments as py_arguments
from ._ast_compat import dump as py_ast_dump
from ._ast_compat import unparse as py_ast_unparse


@dataclass(frozen=True)
class PythonTranslation(ABCTranslation[py.Module, py.stmt, py.expr]):
    builtinInterpreter: BuiltinInterpreter[Any, Any, Any, Any, Any]
    moduleHeader: ClassVar[Sequence[py.stmt]] = []
    moduleFooter: ClassVar[Sequence[py.stmt]] = []

    def compile(
        self,
        program: vcl.Program,
        filename: str,
        declaration_context: Dict[str, Any] = {},
    ) -> Dict[str, Any]:
        py_ast = self.translate_program(program)
        try:
            declaration_context["builtinInterpreter"] = self.builtinInterpreter
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
                *self.__class__.moduleHeader,
                *map(self.translate_declaration, program.declarations),
                *self.__class__.moduleFooter,
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
                msg=py.Str(
                    s=f"The postulate {declaration.name} is undefined",
                    **asdict(declaration.provenance),
                ),
            )
        )

    def translate_DefFunction(self, declaration: vcl.DefFunction) -> py.stmt:
        return py.Assign(
            targets=[py.Name(id=declaration.name, ctx=py.Load())],
            value=self.translate_expression(declaration.body),
            **asdict(declaration.provenance),
        )

    def translate_App(self, expression: vcl.App) -> py.expr:
        return py.Call(
            func=self.translate_expression(expression.func),
            args=[self.translate_expression(arg) for arg in expression.args],
            keywords=[],
            **asdict(expression.provenance),
        )

    def translate_BoundVar(self, expression: vcl.BoundVar) -> py.expr:
        return py.Name(
            id=expression.name,
            ctx=py.Load(),
            **asdict(expression.provenance),
        )

    def translate_BuiltinOp(self, expression: vcl.BuiltinOp) -> py.expr:
        # Make the arguments for 'interpret_*':
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
        # Make the function call to 'interpret_*':
        return py.Call(
            func=py.Attribute(
                value=py.Name(
                    id="builtinInterpreter",
                    ctx=py.Load(),
                    **asdict(expression.provenance),
                ),
                attr=f"interpret_{expression.builtin.__class__.__name__}",
                ctx=py.Load(),
                **asdict(expression.provenance),
            ),
            args=[],
            keywords=keywords,
            **asdict(expression.provenance),
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


def py_binder(*args: py.arg) -> py.arguments:
    return py_arguments(
        posonlyargs=[],
        args=list(args),
        vararg=None,
        kwonlyargs=[],
        kw_defaults=[],
        kwarg=None,
        defaults=[],
    )
