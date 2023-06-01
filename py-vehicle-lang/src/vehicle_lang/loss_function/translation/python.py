import ast as py
from typing import Any, Dict, cast

from typing_extensions import assert_never, override

from ..._compat_singledispatchmethod import singledispatchmethod
from .. import Declaration, Expression, Module
from .. import _ast as vcl
from .. import provenance
from . import Translation


class PythonTranslation(Translation[py.Module, py.stmt, py.expr]):
    def compile_module(self, module: Module, filename: str) -> Dict[str, Any]:
        decl_ctx: Dict[str, Any] = {}
        ast_node = self.translate_module(module)
        bytecode = compile(ast_node, filename=filename, mode="exec")
        exec(bytecode, decl_ctx)
        return decl_ctx

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
        return cast(py.stmt, self._translate_declaration(declaration))

    @singledispatchmethod
    def _translate_declaration(self, node: Declaration) -> py.stmt:
        raise TypeError(f"Unexpected declaration {repr(node)}")

    @_translate_declaration.register
    def _(self, node: vcl.DefFunction) -> py.stmt:
        return py.Assign(
            [py.Name(node.declaration_name, py.Store(), **provenance(node))],
            self.translate_expression(node.declaration_body),
            **provenance(node),
        )

    @override
    def translate_expression(self, expression: Expression) -> py.expr:
        return cast(py.expr, self._translate_expression(expression))

    @singledispatchmethod
    def _translate_expression(self, node: Expression) -> py.expr:
        raise TypeError(f"Unexpected expression {repr(node)}")

    @_translate_expression.register
    def _(self, node: vcl.UnaryOperator) -> py.expr:
        arg = self.translate_expression(node.unary_operator_arg)
        oper = node.unary_operator_name
        if oper == "negation":
            return py.UnaryOp(py.USub(), arg, **provenance(node))
        assert_never()
        raise TypeError(f"Unexpected unary operator {oper}")

    @_translate_expression.register
    def _(self, node: vcl.BinaryOperator) -> py.expr:
        arg1 = self.translate_expression(node.binary_operator_arg1)
        arg2 = self.translate_expression(node.binary_operator_arg2)
        oper = node.binary_operator_name
        if oper == "addition":
            return py.BinOp(arg1, py.Add(), arg2, **provenance(node))
        if oper == "subtraction":
            return py.BinOp(arg1, py.Sub(), arg2, **provenance(node))
        if oper == "multiplication":
            return py.BinOp(arg1, py.Mult(), arg2, **provenance(node))
        if oper == "division":
            return py.BinOp(arg1, py.Div(), arg2, **provenance(node))
        if oper == "power":
            return py.BinOp(arg1, py.Pow(), arg2, **provenance(node))
        if oper == "at":
            return py.Subscript(arg1, arg2, py.Load, **provenance(node))
        if oper == "min":
            min_name = py.Name("min", py.Load, **provenance(node))
            return py.Call(min_name, [arg1, arg2], [], **provenance(node))
        if oper == "max":
            max_name = py.Name("max", py.Load, **provenance(node))
            return py.Call(max_name, [arg1, arg2], [], **provenance(node))
        if oper == "indicator_function":
            return py.IfExp(
                py.BoolOp(py.Eq(), arg1, arg2, **provenance(node)),
                py.Num(1, **provenance(node)),
                py.Num(0, **provenance(node)),
                **provenance(node),
            )
        if oper == "map":
            map_name = py.Name("map", py.Load)
            return py.Call(map_name, [arg1, arg2], [], **provenance(node))
        assert_never()
        raise TypeError(f"Unexpected binary operator {oper}")

    @_translate_expression.register
    def _(self, node: vcl.Constant) -> py.expr:
        return py.Num(node.constant_value, **provenance(node))

    @_translate_expression.register
    def _(self, node: vcl.Variable) -> py.expr:
        return py.Name(node.variable_name, py.Load(), **provenance(node))

    @_translate_expression.register
    def _(self, node: vcl.FreeVariable) -> py.expr:
        func = py.Name(node.function_name, py.Load(), **provenance(node))
        return py.Call(
            func,
            [self.translate_expression(arg) for arg in node.function_args],
            [],
            **provenance(node),
        )

    @_translate_expression.register
    def _(
        self,
        node: vcl.NetworkApplication,
    ) -> py.expr:
        raise NotImplementedError("NetworkApplication is not implemented")

    @_translate_expression.register
    def _(self, node: vcl.Quantifier) -> py.expr:
        raise NotImplementedError("Quantifier is not implemented")

    @_translate_expression.register
    def _(self, node: vcl.TensorLiteral) -> py.expr:
        return py.Tuple(
            *(self.translate_expression(expression) for expression in node.sequence),
            py.Load(),
        )

    @_translate_expression.register
    def _(self, node: vcl.Let) -> py.expr:
        lam_name = py.Name(node.let_bound_name, py.Load(), **provenance(node))
        lam_body = self.translate_expression(node.let_body)
        lam = py.Lambda(lam_name, lam_body, **provenance(node))
        let_expr = self.translate_expression(node.let_bound_expr)
        return py.Call(lam, [let_expr], [], **provenance(node))

    @_translate_expression.register
    def _(self, node: vcl.Lambda) -> py.expr:
        lam_name = py.Name(node.lambda_bound_name, py.Load(), **provenance(node))
        lam_body = self.translate_expression(node.lambda_body)
        return py.Lambda(lam_name, lam_body, **provenance(node))

    @_translate_expression.register
    def _(self, node: vcl.Range) -> py.expr:
        raise NotImplementedError("Range is not implemented")

    @_translate_expression.register
    def _(self, node: vcl.ExponentialAnd) -> py.expr:
        raise NotImplementedError("ExponentialAnd is not implemented")
