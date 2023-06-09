from abc import ABCMeta, abstractmethod
from typing import Generic, Iterable

from typing_extensions import TypeVar, override

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

_Module = TypeVar("_Module")
_Declaration = TypeVar("_Declaration")
_Expression = TypeVar("_Expression")


class Translation(Generic[_Module, _Declaration, _Expression], metaclass=ABCMeta):
    @abstractmethod
    def translate_module(self, module: Module) -> _Module:
        ...

    @abstractmethod
    def translate_declaration(self, declaration: Declaration) -> _Declaration:
        ...

    @abstractmethod
    def translate_expression(self, expression: Expression) -> _Expression:
        ...


class ABCTranslation(Translation[_Module, _Declaration, _Expression]):
    @override
    def translate_declaration(self, declaration: Declaration) -> _Declaration:
        if isinstance(declaration, DefFunction):
            return self.translate_DefFunction(declaration)
        raise NotImplementedError(type(declaration).__name__)

    def translate_declarations(
        self, declarations: Iterable[Declaration]
    ) -> Iterable[_Declaration]:
        return map(self.translate_declaration, declarations)

    @abstractmethod
    def translate_DefFunction(self, declaration: DefFunction) -> _Declaration:
        ...

    @override
    def translate_expression(self, expression: Expression) -> _Expression:
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

    def translate_expressions(
        self, expressions: Iterable[Expression]
    ) -> Iterable[_Expression]:
        return map(self.translate_expression, expressions)

    @abstractmethod
    def translate_Negation(self, expression: Negation) -> _Expression:
        ...

    @abstractmethod
    def translate_Constant(self, _expression: Constant) -> _Expression:
        ...

    @abstractmethod
    def translate_Min(self, expression: Min) -> _Expression:
        ...

    @abstractmethod
    def translate_Max(self, expression: Max) -> _Expression:
        ...

    @abstractmethod
    def translate_Addition(self, expression: Addition) -> _Expression:
        ...

    @abstractmethod
    def translate_Subtraction(self, expression: Subtraction) -> _Expression:
        ...

    @abstractmethod
    def translate_Multiplication(self, expression: Multiplication) -> _Expression:
        ...

    @abstractmethod
    def translate_Division(self, expression: Division) -> _Expression:
        ...

    @abstractmethod
    def translate_IndicatorFunction(self, expression: IndicatorFunction) -> _Expression:
        ...

    @abstractmethod
    def translate_Variable(self, _expression: Variable) -> _Expression:
        ...

    @abstractmethod
    def translate_FreeVariable(self, expression: FreeVariable) -> _Expression:
        ...

    @abstractmethod
    def translate_NetworkApplication(
        self, expression: NetworkApplication
    ) -> _Expression:
        ...

    @abstractmethod
    def translate_Quantifier(self, expression: Quantifier) -> _Expression:
        ...

    @abstractmethod
    def translate_At(self, expression: At) -> _Expression:
        ...

    @abstractmethod
    def translate_TensorLiteral(self, expression: TensorLiteral) -> _Expression:
        ...

    @abstractmethod
    def translate_Lambda(self, expression: Lambda) -> _Expression:
        ...

    @abstractmethod
    def translate_Let(self, expression: Let) -> _Expression:
        ...

    @abstractmethod
    def translate_Power(self, expression: Power) -> _Expression:
        ...

    @abstractmethod
    def translate_Range(self, expression: Range) -> _Expression:
        ...

    @abstractmethod
    def translate_Map(self, expression: Map) -> _Expression:
        ...

    @abstractmethod
    def translate_ExponentialAnd(self, expression: ExponentialAnd) -> _Expression:
        ...
