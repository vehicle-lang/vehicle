from abc import ABCMeta, abstractmethod
from typing import Generic

from typing_extensions import override

from ... import ast as vcl_ast
from . import types as vcl_var

################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


class Translation(
    Generic[vcl_var.Program, vcl_var.Declaration, vcl_var.Expression], metaclass=ABCMeta
):
    @abstractmethod
    def translate_program(self, program: vcl_ast.Program) -> vcl_var.Program: ...

    @abstractmethod
    def translate_declaration(
        self, declaration: vcl_ast.Declaration
    ) -> vcl_var.Declaration: ...

    @abstractmethod
    def translate_expression(
        self, expression: vcl_ast.Expression
    ) -> vcl_var.Expression: ...


class ABCTranslation(
    Translation[vcl_var.Program, vcl_var.Declaration, vcl_var.Expression]
):
    @override
    def translate_program(self, program: vcl_ast.Program) -> vcl_var.Program:
        if isinstance(program, vcl_ast.Main):
            return self.translate_Main(program)
        raise NotImplementedError(type(program).__name__)

    @abstractmethod
    def translate_Main(self, program: vcl_ast.Main) -> vcl_var.Program: ...

    @override
    def translate_declaration(
        self, declaration: vcl_ast.Declaration
    ) -> vcl_var.Declaration:
        if isinstance(declaration, vcl_ast.DefFunction):
            return self.translate_DefFunction(declaration)
        if isinstance(declaration, vcl_ast.DefPostulate):
            return self.translate_DefPostulate(declaration)
        raise NotImplementedError(type(declaration).__name__)

    @abstractmethod
    def translate_DefFunction(
        self, declaration: vcl_ast.DefFunction
    ) -> vcl_var.Declaration: ...

    @abstractmethod
    def translate_DefPostulate(
        self, declaration: vcl_ast.DefPostulate
    ) -> vcl_var.Declaration: ...

    @override
    def translate_expression(
        self, expression: vcl_ast.Expression
    ) -> vcl_var.Expression:
        if isinstance(expression, vcl_ast.App):
            return self.translate_App(expression)
        if isinstance(expression, vcl_ast.Var):
            return self.translate_Var(expression)
        if isinstance(expression, vcl_ast.Builtin):
            return self.translate_Builtin(expression)
        if isinstance(expression, vcl_ast.Lam):
            return self.translate_Lam(expression)
        if isinstance(expression, vcl_ast.PartialApp):
            return self.translate_PartialApp(expression)
        if isinstance(expression, vcl_ast.Pi):
            return self.translate_Pi(expression)
        raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: vcl_ast.App) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Var(self, expression: vcl_ast.Var) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Builtin(self, expression: vcl_ast.Builtin) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Lam(self, expression: vcl_ast.Lam) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_PartialApp(
        self, expression: vcl_ast.PartialApp
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Pi(self, expression: vcl_ast.Pi) -> vcl_var.Expression: ...
