from abc import ABCMeta, abstractmethod
from typing import Generic

from typing_extensions import override
from vehicle_lang.compile.abc import types as vcl_var

# from ... import ast as vcl_ast
from vehicle_lang.compile.ast import nodes as vcl_ast

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
        match program:
            case vcl_ast.Main():
                return self.translate_Main(program)
            case _:
                raise NotImplementedError(type(program).__name__)

    @abstractmethod
    def translate_Main(self, program: vcl_ast.Main) -> vcl_var.Program: ...

    @override
    def translate_declaration(
        self, declaration: vcl_ast.Declaration
    ) -> vcl_var.Declaration:
        match declaration:
            case vcl_ast.DefFunction():
                return self.translate_DefFunction(declaration)
            case _:
                raise NotImplementedError(type(declaration).__name__)

    @abstractmethod
    def translate_DefFunction(
        self, declaration: vcl_ast.DefFunction
    ) -> vcl_var.Declaration: ...

    @override
    def translate_expression(
        self, expression: vcl_ast.Expression
    ) -> vcl_var.Expression:
        match expression:
            case vcl_ast.App():
                return self.translate_App(expression)
            case vcl_ast.Var():
                return self.translate_Var(expression)
            case vcl_ast.Lam():
                return self.translate_Lam(expression)
            case vcl_ast.PartialApp():
                return self.translate_PartialApp(expression)
            case vcl_ast.Pi():
                return self.translate_Pi(expression)
            case vcl_ast.RatTensor():
                return self.translate_RatTensor(expression)
            case vcl_ast.AddRatTensor():
                return self.translate_AddRatTensor(expression)
            case vcl_ast.SubRatTensor():
                return self.translate_SubRatTensor(expression)
            case vcl_ast.MulRatTensor():
                return self.translate_MulRatTensor(expression)
            case vcl_ast.DivRatTensor():
                return self.translate_DivRatTensor(expression)
            case vcl_ast.MinRatTensor():
                return self.translate_MinRatTensor(expression)
            case vcl_ast.MaxRatTensor():
                return self.translate_MaxRatTensor(expression)
            case vcl_ast.NegRatTensor():
                return self.translate_NegRatTensor(expression)
            case vcl_ast.ReduceAddRatTensor():
                return self.translate_ReduceAddRatTensor(expression)
            case vcl_ast.ReduceMulRatTensor():
                return self.translate_ReduceMulRatTensor(expression)
            case vcl_ast.ReduceMinRatTensor():
                return self.translate_ReduceMinRatTensor(expression)
            case vcl_ast.ReduceMaxRatTensor():
                return self.translate_ReduceMaxRatTensor(expression)
            case vcl_ast.SearchRatTensor():
                return self.translate_SearchRatTensor(expression)
            case vcl_ast.Dimension():
                return self.translate_Dimension(expression)
            case vcl_ast.DimensionLookup():
                return self.translate_DimensionLookup(expression)
            case vcl_ast.DimensionCons():
                return self.translate_DimensionCons(expression)
            case vcl_ast.DimensionIndex():
                return self.translate_DimensionIndex(expression)
            case vcl_ast.DimensionNil():
                return self.translate_DimensionNil(expression)
            case vcl_ast.ConstTensor():
                return self.translate_ConstTensor(expression)
            case vcl_ast.StackTensor():
                return self.translate_StackTensor(expression)
            case _:
                raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: vcl_ast.App) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Var(self, expression: vcl_ast.Var) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Lam(self, expression: vcl_ast.Lam) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_PartialApp(
        self, expression: vcl_ast.PartialApp
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Pi(self, expression: vcl_ast.Pi) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_RatTensor(
        self, expression: vcl_ast.RatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_AddRatTensor(
        self, expression: vcl_ast.AddRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_SubRatTensor(
        self, expression: vcl_ast.SubRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_MulRatTensor(
        self, expression: vcl_ast.MulRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_DivRatTensor(
        self, expression: vcl_ast.DivRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_MinRatTensor(
        self, expression: vcl_ast.MinRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_MaxRatTensor(
        self, expression: vcl_ast.MaxRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_NegRatTensor(
        self, expression: vcl_ast.NegRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ReduceAddRatTensor(
        self, expression: vcl_ast.ReduceAddRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ReduceMulRatTensor(
        self, expression: vcl_ast.ReduceMulRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ReduceMinRatTensor(
        self, expression: vcl_ast.ReduceMinRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ReduceMaxRatTensor(
        self, expression: vcl_ast.ReduceMaxRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_SearchRatTensor(
        self, expression: vcl_ast.SearchRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Dimension(
        self, expression: vcl_ast.Dimension
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_DimensionNil(
        self, expression: vcl_ast.DimensionNil
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_DimensionLookup(
        self, expression: vcl_ast.DimensionLookup
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_DimensionCons(
        self, expression: vcl_ast.DimensionCons
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_DimensionIndex(
        self, expression: vcl_ast.DimensionIndex
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ConstTensor(
        self, expression: vcl_ast.ConstTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_StackTensor(
        self, expression: vcl_ast.StackTensor
    ) -> vcl_var.Expression: ...
