import ast as py
from dataclasses import asdict
from typing import Any, Dict

from typing_extensions import override

from .. import Declaration, Expression, Module
from .. import _ast as vcl
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
from . import Translation


class PythonTranslation(Translation[py.Module, py.stmt, py.expr]):
    def compile(
        self, module: Module, filename: str, declaration_context: Dict[str, Any] = {}
    ) -> Dict[str, Any]:
        py_ast = self.translate_module(module)
        py_bytecode = compile(py_ast, filename=filename, mode="exec")
        exec(py_bytecode, declaration_context)
        return declaration_context

    @override
    def translate_module(self, module: Module) -> py.Module:
        return py.Module(
            body=[
                self.translate_declaration(declaration)
                for declaration in module.declarations
            ],
            type_ignores=[],
        )

    @override
    def translate_declaration(self, declaration: Declaration) -> py.stmt:
        if isinstance(declaration, DefFunction):
            provenance = asdict(declaration.provenance)
            return py.Assign(
                [py.Name(declaration.name, py.Store(), **provenance)],
                self.translate_expression(declaration.body),
                **provenance,
            )

        raise NotImplementedError(type(declaration).__name__)

    @override
    def translate_expression(self, expression: Expression) -> py.expr:
        if isinstance(expression, Negation):
            operand = self.translate_expression(expression.operand)
            provenance = asdict(expression.provenance)
            return py.UnaryOp(py.USub(), operand, **provenance)

        if isinstance(expression, Constant):
            provenance = asdict(expression.provenance)
            return py.Num(expression.value, **provenance)

        if isinstance(expression, Min):
            provenance = asdict(expression.provenance)
            func = py.Name("min", py.Load, **provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.Call(func, [left, right], [], **provenance)

        if isinstance(expression, Max):
            provenance = asdict(expression.provenance)
            func = py.Name("max", py.Load, **provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.Call(func, [left, right], [], **provenance)

        if isinstance(expression, Addition):
            provenance = asdict(expression.provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.BinOp(left, py.Add(), right, **provenance)

        if isinstance(expression, Subtraction):
            provenance = asdict(expression.provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.BinOp(left, py.Sub(), right, **provenance)

        if isinstance(expression, Multiplication):
            provenance = asdict(expression.provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.BinOp(left, py.Mult(), right, **provenance)

        if isinstance(expression, Division):
            provenance = asdict(expression.provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.BinOp(left, py.Div(), right, **provenance)

        if isinstance(expression, IndicatorFunction):
            provenance = asdict(expression.provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.IfExp(
                py.BoolOp(py.Eq(), left, right, **provenance),
                py.Num(1, **provenance),
                py.Num(0, **provenance),
                **provenance,
            )

        if isinstance(expression, Variable):
            provenance = asdict(expression.provenance)
            return py.Name(expression.name, py.Load(), **provenance)

        if isinstance(expression, FreeVariable):
            provenance = asdict(expression.provenance)
            func = py.Name(expression.func, py.Load(), **provenance)
            args = [self.translate_expression(arg) for arg in expression.args]
            return py.Call(func, args, [], **provenance)

        if isinstance(expression, NetworkApplication):
            provenance = asdict(expression.provenance)
            func = py.Name(expression.func, py.Load(), **provenance)
            args = [self.translate_expression(arg) for arg in expression.args]
            return py.Call(func, args, [], **provenance)

        if isinstance(expression, Quantifier):
            provenance = asdict(expression.provenance)
            raise NotImplementedError("Quantifier")

        if isinstance(expression, At):
            provenance = asdict(expression.provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.Subscript(left, right, py.Load(), **provenance)

        if isinstance(expression, TensorLiteral):
            provenance = asdict(expression.provenance)
            sequence = [self.translate_expression(item) for item in expression.sequence]
            return py.List(*sequence, py.Load())

        if isinstance(expression, Lambda):
            provenance = asdict(expression.provenance)
            name = py.Name(expression.name, py.Load(), **provenance)
            body = self.translate_expression(expression.body)
            return py.Lambda(name, body, **provenance)

        if isinstance(expression, Let):
            provenance = asdict(expression.provenance)
            name = py.Name(expression.name, py.Load(), **provenance)
            value = self.translate_expression(expression.value)
            body = self.translate_expression(expression.body)
            lam = py.Lambda(name, body, **provenance)
            return py.Call(lam, [value], [], **provenance)

        if isinstance(expression, Power):
            provenance = asdict(expression.provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.BinOp(left, py.Pow(), right, **provenance)

        if isinstance(expression, Range):
            provenance = asdict(expression.provenance)
            raise NotImplementedError("Range")

        if isinstance(expression, Map):
            provenance = asdict(expression.provenance)
            func = py.Name("map", py.Load, **provenance)
            left = self.translate_expression(expression.left)
            right = self.translate_expression(expression.right)
            return py.Call(func, [left, right], [], **provenance)

        if isinstance(expression, ExponentialAnd):
            provenance = asdict(expression.provenance)
            raise NotImplementedError("ExponentialAnd")

        raise NotImplementedError(type(expression).__name__)
