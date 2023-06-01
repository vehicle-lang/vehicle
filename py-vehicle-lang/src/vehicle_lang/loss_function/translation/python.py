import ast
from typing import Any, Dict, cast

import ast_compat
from typing_extensions import assert_never, override

from ..._compat_singledispatchmethod import singledispatchmethod
from .. import Declaration, Expression, Module
from .. import _ast as vcl
from . import Translation


class PythonTranslation(Translation[ast.Module, ast.stmt, ast.expr]):
    def compile_module(self, module: Module, filename: str) -> Dict[str, Any]:
        ctx_dict: Dict[str, Any] = {}
        ast_node = self.translate_module(module)
        bytecode = compile(ast_node, filename=filename, mode="exec")
        exec(bytecode, ctx_dict)
        return ctx_dict

    @override
    def translate_module(self, module: Module) -> ast.Module:
        return ast.Module(
            *(
                self.translate_declaration(declaration)
                for declaration in module.declarations
            )
        )

    @override
    def translate_declaration(self, declaration: Declaration) -> ast.stmt:
        return cast(ast.stmt, self._translate_declaration(declaration))

    @singledispatchmethod
    def _translate_declaration(self, declaration: Declaration) -> ast.stmt:
        raise TypeError(f"Unexpected declaration {repr(declaration)}")

    @_translate_declaration.register
    def _(self, def_function: vcl.DefFunction) -> ast.stmt:
        return ast.Assign(
            ast.Name(def_function.declaration_name, ast.Store),
            self.translate_expression(def_function.declaration_body),
        )

    @override
    def translate_expression(self, expression: Expression) -> ast.expr:
        return cast(ast.expr, self._translate_expression(expression))

    @singledispatchmethod
    def _translate_expression(self, expression: Expression) -> ast.expr:
        raise TypeError(f"Unexpected expression {repr(expression)}")

    @_translate_expression.register
    def _(self, unary_operator: vcl.UnaryOperator) -> ast.expr:
        arg = self.translate_expression(unary_operator.unary_operator_arg)
        oper = unary_operator.unary_operator_name
        if oper == "negation":
            return ast.UnaryOp(ast.USub(), arg)
        assert_never()
        raise TypeError(f"Unexpected unary operator {oper}")

    @_translate_expression.register
    def _(self, binary_operator: vcl.BinaryOperator) -> ast.expr:
        arg1 = self.translate_expression(binary_operator.binary_operator_arg1)
        arg2 = self.translate_expression(binary_operator.binary_operator_arg2)
        oper = binary_operator.binary_operator_name
        if oper == "addition":
            return ast.BinOp(arg1, ast.Add(), arg2)
        if oper == "subtraction":
            return ast.BinOp(arg1, ast.Sub(), arg2)
        if oper == "multiplication":
            return ast.BinOp(arg1, ast.Mult(), arg2)
        if oper == "division":
            return ast.BinOp(arg1, ast.Div(), arg2)
        if oper == "power":
            return ast.BinOp(arg1, ast.Pow(), arg2)
        if oper == "at":
            return ast.Subscript(arg1, arg2, ast.Load)
        if oper == "min":
            min_name = ast.Name("min", ast.Load)
            return ast.Call(min_name, [arg1, arg2], [])
        if oper == "max":
            max_name = ast.Name("max", ast.Load)
            return ast.Call(max_name, [arg1, arg2], [])
        if oper == "indicator_function":
            return ast.IfExp(
                ast.BoolOp(ast.Eq(), arg1, arg2),
                ast_compat.Constant(1),
                ast_compat.Constant(0),
            )
        if oper == "map":
            map_name = ast.Name("map", ast.Load)
            return ast.Call(map_name, [arg1, arg2], [])
        assert_never()
        raise TypeError(f"Unexpected binary operator {oper}")

    @_translate_expression.register
    def _(self, constant: vcl.Constant) -> ast.expr:
        return cast(ast.expr, ast_compat.Constant(constant.constant_value))

    @_translate_expression.register
    def _(self, variable: vcl.Variable) -> ast.expr:
        return ast.Name(variable.variable_name, ast.Load())

    @_translate_expression.register
    def _(self, free_variable: vcl.FreeVariable) -> ast.expr:
        func_name = ast.Name(free_variable.function_name, ast.Load())
        return ast.Call(
            func_name,
            [self.translate_expression(arg) for arg in free_variable.function_args],
            [],
        )

    @_translate_expression.register
    def _(
        self,
        network_application: vcl.NetworkApplication,
    ) -> ast.expr:
        raise NotImplementedError("NetworkApplication is not implemented")

    @_translate_expression.register
    def _(self, quantifier: vcl.Quantifier) -> ast.expr:
        raise NotImplementedError("Quantifier is not implemented")

    @_translate_expression.register
    def _(self, tensor_literal: vcl.TensorLiteral) -> ast.expr:
        return ast.Tuple(
            *(
                self.translate_expression(expression)
                for expression in tensor_literal.sequence
            ),
            ast.Load,
        )

    @_translate_expression.register
    def _(self, let: vcl.Let) -> ast.expr:
        lam_name = ast.Name(let.let_bound_name, ast.Load)
        lam_body = self.translate_expression(let.let_body)
        lam = ast.Lambda(lam_name, lam_body)
        let_expr = self.translate_expression(let.let_bound_expr)
        return ast.Call(lam, [let_expr], [])

    @_translate_expression.register
    def _(self, lam: vcl.Lambda) -> ast.expr:
        lam_name = ast.Name(lam.lambda_bound_name, ast.Load)
        lam_body = self.translate_expression(lam.lambda_body)
        return ast.Lambda(lam_name, lam_body)

    @_translate_expression.register
    def _(self, range: vcl.Range) -> ast.expr:
        raise NotImplementedError("Range is not implemented")

    @_translate_expression.register
    def _(self, exponential_and: vcl.ExponentialAnd) -> ast.expr:
        raise NotImplementedError("ExponentialAnd is not implemented")
