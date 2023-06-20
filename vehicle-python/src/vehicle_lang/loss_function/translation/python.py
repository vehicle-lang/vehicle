import ast as py
from dataclasses import asdict, dataclass
from typing import Any, ClassVar, Dict, Sequence

from typing_extensions import override

from .. import Module
from .._ast import (
    Addition,
    At,
    Constant,
    DefFunction,
    Division,
    ExponentialAnd,
    FreeVariable,
    IndicatorFunction,
    Lambda,
    Let,
    Map,
    Max,
    Min,
    Multiplication,
    Negation,
    NetworkApplication,
    Power,
    Quantifier,
    Range,
    Subtraction,
    TensorLiteral,
    Variable,
)
from . import ABCTranslation
from ._ast_compat import arguments as py_arguments
from ._ast_compat import dump as py_ast_dump
from ._ast_compat import unparse as py_ast_unparse


@dataclass(frozen=True)
class PythonTranslation(ABCTranslation[py.Module, py.stmt, py.expr]):
    def compile(
        self, module: Module, filename: str, declaration_context: Dict[str, Any] = {}
    ) -> Dict[str, Any]:
        py_ast = self.translate_module(module)
        try:
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

    @override
    def translate_module(self, module: Module) -> py.Module:
        return py.Module(
            body=[
                *self.get_module_header(),
                *self.translate_declarations(module.declarations),
                *self.get_module_footer(),
            ],
            type_ignores=[],
        )

    def get_module_header(self) -> Sequence[py.stmt]:
        return []

    def get_module_footer(self) -> Sequence[py.stmt]:
        return []

    @override
    def translate_DefFunction(self, declaration: DefFunction) -> py.stmt:
        provenance = asdict(declaration.provenance)
        return py.FunctionDef(
            declaration.name,
            py_arguments([], [], None, [], [], None, []),
            [py.Return(self.translate_expression(declaration.body), **provenance)],
            [],
            None,
            **provenance,
        )

    @override
    def translate_Negation(self, expression: Negation) -> py.expr:
        operand = self.translate_expression(expression.operand)
        provenance = asdict(expression.provenance)
        return py.UnaryOp(py.USub(), operand, **provenance)

    @override
    def translate_Constant(self, expression: Constant) -> py.expr:
        provenance = asdict(expression.provenance)
        return py.Num(expression.value, **provenance)

    @override
    def translate_Min(self, expression: Min) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name("min", py.Load(), **provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Call(func, [left, right], [], **provenance)

    @override
    def translate_Max(self, expression: Max) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name("max", py.Load(), **provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Call(func, [left, right], [], **provenance)

    @override
    def translate_Addition(self, expression: Addition) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Add(), right, **provenance)

    @override
    def translate_Subtraction(self, expression: Subtraction) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Sub(), right, **provenance)

    @override
    def translate_Multiplication(self, expression: Multiplication) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Mult(), right, **provenance)

    @override
    def translate_Division(self, expression: Division) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Div(), right, **provenance)

    @override
    def translate_IndicatorFunction(self, expression: IndicatorFunction) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.IfExp(
            py.Compare(left, [py.Eq()], [right], **provenance),
            py.Num(1, **provenance),
            py.Num(0, **provenance),
            **provenance,
        )

    @override
    def translate_Variable(self, expression: Variable) -> py.expr:
        provenance = asdict(expression.provenance)
        return py.Name(expression.name, py.Load(), **provenance)

    @override
    def translate_FreeVariable(self, expression: FreeVariable) -> py.expr:
        # provenance = asdict(expression.provenance)
        # func = py.Name(expression.func, py.Load(), **provenance)
        # args = list(self.translate_expressions(expression.args))
        # return py.Call(func, args, [], **provenance)
        raise NotImplementedError("FreeVariable")

    @override
    def translate_NetworkApplication(self, expression: NetworkApplication) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name(expression.func, py.Load(), **provenance)
        args = list(self.translate_expressions(expression.args))
        return py.Call(func, args, [], **provenance)

    @override
    def translate_Quantifier(self, expression: Quantifier) -> py.expr:
        provenance = asdict(expression.provenance)
        locals_name = py.Name("locals", py.Load(), **provenance)
        locals_call = py.Call(locals_name, [], [], **provenance)
        sampler_name_str = f"sampler_for_{expression.name}"
        sampler_name = py.Name(sampler_name_str, py.Load(), **provenance)
        sampler_args = py.keyword(value=locals_call, **provenance)
        sampler_call = py.Call(sampler_name, [], [sampler_args], **provenance)
        body = self.translate_expression(
            Lambda(expression.name, expression.body, expression.provenance)
        )
        map_name = py.Name("map", py.Load(), **provenance)
        map_call = py.Call(map_name, [body, sampler_call], [], **provenance)
        agg_name = py.Name(
            {"All": "max", "Any": "min"}[expression.kind], py.Load(), **provenance
        )
        agg_call = py.Call(agg_name, [map_call], [], **provenance)
        return agg_call

    @override
    def translate_At(self, expression: At) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Subscript(
            left, py.Index(right, **provenance), py.Load(), **provenance
        )

    @override
    def translate_TensorLiteral(self, expression: TensorLiteral) -> py.expr:
        provenance = asdict(expression.provenance)
        sequence = list(self.translate_expressions(expression.sequence))
        return py.List(sequence, py.Load(), **provenance)

    @override
    def translate_Lambda(self, expression: Lambda) -> py.expr:
        provenance = asdict(expression.provenance)
        body = self.translate_expression(expression.body)
        return py.Lambda(
            py_arguments(
                [], [py.arg(expression.name, **provenance)], None, [], [], None, []
            ),
            body,
            **provenance,
        )

    @override
    def translate_Let(self, expression: Let) -> py.expr:
        provenance = asdict(expression.provenance)
        value = self.translate_expression(expression.value)
        body = self.translate_expression(expression.body)
        lam = py.Lambda(
            py_arguments(
                [], [py.arg(expression.name, **provenance)], None, [], [], None, []
            ),
            body,
            **provenance,
        )
        return py.Call(lam, [value], [], **provenance)

    @override
    def translate_Power(self, expression: Power) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Pow(), right, **provenance)

    @override
    def translate_Range(self, expression: Range) -> py.expr:
        # provenance = asdict(expression.provenance)
        raise NotImplementedError("Range")

    @override
    def translate_Map(self, expression: Map) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name("map", py.Load(), **provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Call(func, [left, right], [], **provenance)

    @override
    def translate_ExponentialAnd(self, expression: ExponentialAnd) -> py.expr:
        # provenance = asdict(expression.provenance)
        raise NotImplementedError("ExponentialAnd")
