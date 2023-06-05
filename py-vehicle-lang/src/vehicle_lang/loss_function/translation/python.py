import ast as py
from dataclasses import asdict, dataclass
from typing import Any, Dict

from typing_extensions import override

from .. import Declaration, Expression, Module
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
from ._ast_compat import arguments as py_arguments
from ._ast_compat import unparse as py_ast_unparse


@dataclass(frozen=True)
class PythonTranslation(Translation[py.Module, py.stmt, py.expr]):
    def compile(
        self, module: Module, filename: str, declaration_context: Dict[str, Any] = {}
    ) -> Dict[str, Any]:
        py_ast = self.translate_module(module)
        try:
            py_bytecode = compile(py_ast, filename=filename, mode="exec")
            exec(py_bytecode, declaration_context)
            return declaration_context
        except TypeError as e:
            raise TypeError(f"{e}:\n{py_ast_unparse(py_ast)}")

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
            return self.translate_DefFunction(declaration)
        raise NotImplementedError(type(declaration).__name__)

    def translate_DefFunction(self, declaration: DefFunction) -> py.stmt:
        provenance = asdict(declaration.provenance)
        return py.Assign(
            [py.Name(declaration.name, py.Store(), **provenance)],
            self.translate_expression(declaration.body),
            **provenance,
        )

    @override
    def translate_expression(self, expression: Expression) -> py.expr:
        if isinstance(expression, Negation):
            return self.translate_Negation(expression)
        if isinstance(expression, Constant):
            return self.translate_Constant(expression)
        if isinstance(expression, Min):
            return self.translate_Min(expression)
        if isinstance(expression, Max):
            return self.translate_Max(expression)
        if isinstance(expression, Addition):
            return self.translate_Addition(expression)
        if isinstance(expression, Subtraction):
            return self.translate_Subtraction(expression)
        if isinstance(expression, Multiplication):
            return self.translate_Multiplication(expression)
        if isinstance(expression, Division):
            return self.translate_Division(expression)
        if isinstance(expression, IndicatorFunction):
            return self.translate_IndicatorFunction(expression)
        if isinstance(expression, Variable):
            return self.translate_Variable(expression)
        if isinstance(expression, FreeVariable):
            return self.translate_FreeVariable(expression)
        if isinstance(expression, NetworkApplication):
            return self.translate_NetworkApplication(expression)
        if isinstance(expression, Quantifier):
            return self.translate_Quantifier(expression)
        if isinstance(expression, At):
            return self.translate_At(expression)
        if isinstance(expression, TensorLiteral):
            return self.translate_TensorLiteral(expression)
        if isinstance(expression, Lambda):
            return self.translate_Lambda(expression)
        if isinstance(expression, Let):
            return self.translate_Let(expression)
        if isinstance(expression, Power):
            return self.translate_Power(expression)
        if isinstance(expression, Range):
            return self.translate_Range(expression)
        if isinstance(expression, Map):
            return self.translate_Map(expression)
        if isinstance(expression, ExponentialAnd):
            return self.translate_ExponentialAnd(expression)
        raise NotImplementedError(type(expression).__name__)

    def translate_Negation(self, expression: Negation) -> py.expr:
        operand = self.translate_expression(expression.operand)
        provenance = asdict(expression.provenance)
        return py.UnaryOp(py.USub(), operand, **provenance)

    def translate_Constant(self, expression: Constant) -> py.expr:
        provenance = asdict(expression.provenance)
        return py.Num(expression.value, **provenance)

    def translate_Min(self, expression: Min) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name("min", py.Load(), **provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Call(func, [left, right], [], **provenance)

    def translate_Max(self, expression: Max) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name("max", py.Load(), **provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Call(func, [left, right], [], **provenance)

    def translate_Addition(self, expression: Addition) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Add(), right, **provenance)

    def translate_Subtraction(self, expression: Subtraction) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Sub(), right, **provenance)

    def translate_Multiplication(self, expression: Multiplication) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Mult(), right, **provenance)

    def translate_Division(self, expression: Division) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Div(), right, **provenance)

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

    def translate_Variable(self, expression: Variable) -> py.expr:
        provenance = asdict(expression.provenance)
        return py.Name(expression.name, py.Load(), **provenance)

    def translate_FreeVariable(self, expression: FreeVariable) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name(expression.func, py.Load(), **provenance)
        args = [self.translate_expression(arg) for arg in expression.args]
        return py.Call(func, args, [], **provenance)

    def translate_NetworkApplication(self, expression: NetworkApplication) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name(expression.func, py.Load(), **provenance)
        args = [self.translate_expression(arg) for arg in expression.args]
        return py.Call(func, args, [], **provenance)

    def translate_Quantifier(self, expression: Quantifier) -> py.expr:
        provenance = asdict(expression.provenance)
        raise NotImplementedError("Quantifier")

    def translate_At(self, expression: At) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Subscript(
            left, py.Index(right, **provenance), py.Load(), **provenance
        )

    def translate_TensorLiteral(self, expression: TensorLiteral) -> py.expr:
        provenance = asdict(expression.provenance)
        sequence = [self.translate_expression(item) for item in expression.sequence]
        return py.List(sequence, py.Load(), **provenance)

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

    def translate_Power(self, expression: Power) -> py.expr:
        provenance = asdict(expression.provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.BinOp(left, py.Pow(), right, **provenance)

    def translate_Range(self, expression: Range) -> py.expr:
        provenance = asdict(expression.provenance)
        raise NotImplementedError("Range")

    def translate_Map(self, expression: Map) -> py.expr:
        provenance = asdict(expression.provenance)
        func = py.Name("map", py.Load, **provenance)
        left = self.translate_expression(expression.left)
        right = self.translate_expression(expression.right)
        return py.Call(func, [left, right], [], **provenance)

    def translate_ExponentialAnd(self, expression: ExponentialAnd) -> py.expr:
        provenance = asdict(expression.provenance)
        raise NotImplementedError("ExponentialAnd")
