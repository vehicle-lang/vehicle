from abc import ABCMeta, abstractmethod
from typing import Generic

from typing_extensions import TypeVar

from .. import Declaration, Expression, Module

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
