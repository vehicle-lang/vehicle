from abc import ABCMeta, abstractmethod
from typing import Generic

from typing_extensions import TypeVar

from .. import Declaration, Expression

_Declaration = TypeVar("_Declaration")
_Expression = TypeVar("_Expression")


class Translation(Generic[_Declaration, _Expression], metaclass=ABCMeta):
    @abstractmethod
    def translate_declaration(self, declaration: Declaration) -> _Declaration:
        ...

    @abstractmethod
    def translate_expressions(self, expression: Expression) -> _Expression:
        ...
