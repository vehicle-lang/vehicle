import json
from abc import ABCMeta
from dataclasses import dataclass
from typing import List, Sequence, Tuple

from typing_extensions import Literal, Self, TypeAlias, override

from ._decode import JsonValue, decode

Name: TypeAlias = str

QuantifierKind: TypeAlias = Literal["Any", "All"]

QuantifierDomain: TypeAlias = Tuple[()]


@dataclass(frozen=True, init=False)
class AST(metaclass=ABCMeta):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class AST")

    @classmethod
    def from_dict(cls, value: JsonValue) -> Self:
        return decode(cls, value)

    @classmethod
    def from_json(cls, value: str) -> Self:
        return cls.from_dict(json.loads(value))


################################################################################
# Expressions
################################################################################


@dataclass(frozen=True, init=False)
class Expression(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Expression")


@dataclass(frozen=True)
class Negation(Expression):
    operand: Expression


@dataclass(frozen=True)
class Constant(Expression):
    value: float


@dataclass(frozen=True)
class Min(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class Max(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class Addition(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class Subtraction(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class Multiplication(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class Division(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class IndicatorFunction(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class Variable(Expression):
    name: Name


@dataclass(frozen=True)
class FreeVariable(Expression):
    func: Name
    args: Sequence[Expression]


@dataclass(frozen=True)
class NetworkApplication(Expression):
    func: Name
    args: Sequence[Expression]


@dataclass(frozen=True)
class Quantifier(Expression):
    kind: QuantifierKind
    name: Name
    domain: QuantifierDomain
    body: Expression


@dataclass(frozen=True)
class At(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class TensorLiteral(Expression):
    sequence: Sequence[Expression]


@dataclass(frozen=True)
class Lambda(Expression):
    name: Name
    body: Expression


@dataclass(frozen=True)
class Let(Expression):
    name: Name
    value: Expression
    body: Expression


@dataclass(frozen=True)
class Power(Expression):
    left: Expression
    right: Expression


@dataclass(frozen=True)
class Range(Expression):
    range: Expression


@dataclass(frozen=True)
class Map(Expression):
    func: Expression
    args: Expression


@dataclass(frozen=True)
class ExponentialAnd(Expression):
    args: Sequence[Expression]


################################################################################
# Declarations
################################################################################


@dataclass(frozen=True, init=False)
class Declaration(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Declaration")


@dataclass(frozen=True)
class DefFunction(Declaration):
    name: Name
    body: Expression


################################################################################
# Modules
################################################################################


@dataclass(frozen=True)
class Module(AST):
    declarations: Sequence[Declaration]

    @override
    @classmethod
    def from_dict(cls, value: JsonValue) -> Self:
        if isinstance(value, List):
            return cls([decode(Declaration, item) for item in value])
        else:
            return super().from_dict(value)
