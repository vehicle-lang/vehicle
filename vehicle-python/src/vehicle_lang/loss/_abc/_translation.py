from abc import ABCMeta, abstractmethod
from typing import Generic

from ..._ast import _nodes as vcl_ast
from . import _types as vcl_var

################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


class ABCTranslation(
    Generic[vcl_var.Program, vcl_var.Declaration, vcl_var.Expression], metaclass=ABCMeta
):
    @abstractmethod
    def translate_Main(self, program: vcl_ast.Main) -> vcl_var.Program: ...

    @abstractmethod
    def translate_DefFunction(
        self, declaration: vcl_ast.DefFunction
    ) -> vcl_var.Declaration: ...

    def translate_program(self, program: vcl_ast.Program) -> vcl_var.Program:
        match program:
            case vcl_ast.Main():
                return self.translate_Main(program)
            case _:
                raise NotImplementedError(type(program).__name__)

    def translate_declaration(
        self, declaration: vcl_ast.Declaration
    ) -> vcl_var.Declaration:
        match declaration:
            case vcl_ast.DefFunction():
                return self.translate_DefFunction(declaration)
            case _:
                raise NotImplementedError(type(declaration).__name__)

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
            case vcl_ast.Pi():
                return self.translate_Pi(expression)
            case vcl_ast.Let():
                return self.translate_Let(expression)
            case vcl_ast.Record():
                return self.translate_Record(expression)
            case vcl_ast.RecordAcc():
                return self.translate_RecordAcc(expression)
            case vcl_ast.BoolTensor():
                return self.translate_BoolTensor(expression)
            case vcl_ast.BoolNot():
                return self.translate_BoolNot(expression)
            case vcl_ast.BoolAnd():
                return self.translate_BoolAnd(expression)
            case vcl_ast.BoolOr():
                return self.translate_BoolOr(expression)
            case vcl_ast.BoolImplies():
                return self.translate_BoolImplies(expression)
            case vcl_ast.BoolCompareIndex():
                return self.translate_BoolCompareIndex(expression)
            case vcl_ast.BoolCompareNat():
                return self.translate_BoolCompareNat(expression)
            case vcl_ast.BoolCompareRatTensor():
                return self.translate_BoolCompareRatTensor(expression)
            case vcl_ast.BoolReduceAnd():
                return self.translate_BoolReduceAnd(expression)
            case vcl_ast.BoolReduceOr():
                return self.translate_BoolReduceOr(expression)
            case vcl_ast.BoolIf():
                return self.translate_BoolIf(expression)
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
            case vcl_ast.PowRatTensor():
                return self.translate_PowRatTensor(expression)
            case vcl_ast.LogRatTensor():
                return self.translate_LogRatTensor(expression)
            case vcl_ast.ExpRatTensor():
                return self.translate_ExpRatTensor(expression)
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
            case vcl_ast.WhereTensor():
                return self.translate_WhereTensor(expression)
            case vcl_ast.Dimension():
                return self.translate_Dimension(expression)
            case vcl_ast.DimensionCons():
                return self.translate_DimensionCons(expression)
            case vcl_ast.DimensionIndex():
                return self.translate_DimensionIndex(expression)
            case vcl_ast.DimensionNil():
                return self.translate_DimensionNil(expression)
            case vcl_ast.ConstTensor():
                return self.translate_ConstTensor(expression)
            case vcl_ast.Transpose():
                return self.translate_Transpose(expression)
            case vcl_ast.StackTensor():
                return self.translate_StackTensor(expression)
            case vcl_ast.AtTensor():
                return self.translate_AtTensor(expression)
            case vcl_ast.ForeachTensor():
                return self.translate_ForeachTensor(expression)
            case vcl_ast.VectorLiteral():
                return self.translate_VectorLiteral(expression)
            case vcl_ast.AtVector():
                return self.translate_AtVector(expression)
            case vcl_ast.ForeachVector():
                return self.translate_ForeachVector(expression)
            case _:
                raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: vcl_ast.App) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Var(self, expression: vcl_ast.Var) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Lam(self, expression: vcl_ast.Lam) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Pi(self, expression: vcl_ast.Pi) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Let(self, expression: vcl_ast.Let) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_Record(self, expression: vcl_ast.Record) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_RecordAcc(
        self, expression: vcl_ast.RecordAcc
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolTensor(
        self, expression: vcl_ast.BoolTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolNot(self, expression: vcl_ast.BoolNot) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolAnd(self, expression: vcl_ast.BoolAnd) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolOr(self, expression: vcl_ast.BoolOr) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolImplies(
        self, expression: vcl_ast.BoolImplies
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolCompareIndex(
        self, expression: vcl_ast.BoolCompareIndex
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolCompareNat(
        self, expression: vcl_ast.BoolCompareNat
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolCompareRatTensor(
        self, expression: vcl_ast.BoolCompareRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolReduceAnd(
        self, expression: vcl_ast.BoolReduceAnd
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolReduceOr(
        self, expression: vcl_ast.BoolReduceOr
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_BoolIf(self, expression: vcl_ast.BoolIf) -> vcl_var.Expression: ...

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
    def translate_PowRatTensor(
        self, expression: vcl_ast.PowRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_LogRatTensor(
        self, expression: vcl_ast.LogRatTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ExpRatTensor(
        self, expression: vcl_ast.ExpRatTensor
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
    def translate_WhereTensor(
        self, expression: vcl_ast.WhereTensor
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

    @abstractmethod
    def translate_Transpose(
        self, expression: vcl_ast.Transpose
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_AtTensor(
        self, expression: vcl_ast.AtTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ForeachTensor(
        self, expression: vcl_ast.ForeachTensor
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_VectorLiteral(
        self, expression: vcl_ast.VectorLiteral
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_AtVector(
        self, expression: vcl_ast.AtVector
    ) -> vcl_var.Expression: ...

    @abstractmethod
    def translate_ForeachVector(
        self, expression: vcl_ast.ForeachVector
    ) -> vcl_var.Expression: ...
