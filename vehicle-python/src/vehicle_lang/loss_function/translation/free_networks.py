import ast as py
from dataclasses import dataclass
from itertools import chain
from typing import Iterable, Set

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
    Name,
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


@dataclass(frozen=True)
class FreeNetworks(ABCTranslation[Set[Name], Set[Name], Set[Name]]):
    @override
    def translate_module(self, module: Module) -> Set[Name]:
        return self.translate_and_chain_declarations(module.declarations)

    def translate_and_chain_declarations(
        self, declarations: Iterable[Declaration]
    ) -> Set[Name]:
        return set(chain.from_iterable(self.translate_declarations(declarations)))

    @override
    def translate_DefFunction(self, declaration: DefFunction) -> Set[Name]:
        return self.translate_expression(declaration.body)

    def translate_and_chain_expressions(
        self, expressions: Iterable[Expression]
    ) -> Set[Name]:
        return set(chain.from_iterable(self.translate_expressions(expressions)))

    @override
    def translate_Negation(self, expression: Negation) -> Set[Name]:
        return self.translate_expression(expression.operand)

    @override
    def translate_Constant(self, _expression: Constant) -> Set[Name]:
        return set()

    @override
    def translate_Min(self, expression: Min) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_Max(self, expression: Max) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_Addition(self, expression: Addition) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_Subtraction(self, expression: Subtraction) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_Multiplication(self, expression: Multiplication) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_Division(self, expression: Division) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_IndicatorFunction(self, expression: IndicatorFunction) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_Variable(self, _expression: Variable) -> Set[Name]:
        return set()

    @override
    def translate_FreeVariable(self, expression: FreeVariable) -> Set[Name]:
        return self.translate_and_chain_expressions(expression.args)

    @override
    def translate_NetworkApplication(self, expression: NetworkApplication) -> Set[Name]:
        args = self.translate_and_chain_expressions(expression.args)
        return args.union([expression.func])

    @override
    def translate_Quantifier(self, expression: Quantifier) -> Set[Name]:
        return self.translate_expression(expression.body)

    @override
    def translate_At(self, expression: At) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_TensorLiteral(self, expression: TensorLiteral) -> Set[Name]:
        return self.translate_and_chain_expressions(expression.sequence)

    @override
    def translate_Lambda(self, expression: Lambda) -> Set[Name]:
        return self.translate_expression(expression.body)

    @override
    def translate_Let(self, expression: Let) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.value, expression.body])

    @override
    def translate_Power(self, expression: Power) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_Range(self, expression: Range) -> Set[Name]:
        return self.translate_expression(expression.range)

    @override
    def translate_Map(self, expression: Map) -> Set[Name]:
        return self.translate_and_chain_expressions([expression.left, expression.right])

    @override
    def translate_ExponentialAnd(self, expression: ExponentialAnd) -> Set[Name]:
        return self.translate_and_chain_expressions(expression.args)
