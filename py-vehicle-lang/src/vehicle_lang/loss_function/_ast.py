import json
from abc import ABCMeta
from dataclasses import asdict, dataclass
from typing import Dict, Sequence, Tuple, Union

from typing_extensions import Literal, Self, TypeAlias

from ..errors import VehicleJsonMalformed
from ._decode import JsonValue, decode

Name: TypeAlias = str

UnaryOperatorName: TypeAlias = Literal["negation"]

BinaryOperatorName: TypeAlias = Literal[
    "min",
    "max",
    "addition",
    "subtraction",
    "multiplication",
    "division",
    "indicator_function",
    "at",
    "power",
    "map",
]

QuantifierName: TypeAlias = Literal[
    "all",
    "any",
]

DomainType: TypeAlias = Tuple[()]


@dataclass(frozen=True)
class Node(metaclass=ABCMeta):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract type Node")

    def to_dict(self) -> JsonValue:
        return asdict(self)

    @classmethod
    def from_dict(cls, value: Dict[str, JsonValue]) -> Self:
        return decode(cls, value)

    def to_json(self) -> str:
        return json.dumps(self.to_dict())

    @classmethod
    def from_json(cls, value: str) -> Self:
        return cls.from_dict(json.loads(value))


@dataclass(frozen=True, init=False)
class Expression(Node):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract type Expression")


@dataclass(frozen=True)
class UnaryOperator(Expression):
    unary_operator_name: UnaryOperatorName
    unary_operator_arg: Expression


@dataclass(frozen=True)
class BinaryOperator(Expression):
    binary_operator_name: BinaryOperatorName
    binary_operator_arg1: Expression
    binary_operator_arg2: Expression


@dataclass(frozen=True)
class Constant(Expression):
    constant_value: Union[int, float, complex]


@dataclass(frozen=True)
class Variable(Expression):
    variable_name: Name


@dataclass(frozen=True)
class FreeVariable(Expression):
    function_name: Name
    function_args: Sequence[Expression]


@dataclass(frozen=True)
class NetworkApplication(Expression):
    network_name: Name
    network_args: Sequence[Expression]


@dataclass(frozen=True)
class Quantifier(Expression):
    quantifier: QuantifierName
    quantifier_bound_name: Name
    quantifier_domain: DomainType
    quantifier_body: Expression


@dataclass(frozen=True)
class TensorLiteral(Expression):
    sequence: Sequence[Expression]


@dataclass(frozen=True)
class Let(Expression):
    let_bound_name: Name
    let_bound_expr: Expression
    let_body: Expression


@dataclass(frozen=True)
class Lambda(Expression):
    lambda_bound_name: Name
    lambda_body: Expression


@dataclass(frozen=True)
class Range(Expression):
    range_expr: Expression


@dataclass(frozen=True)
class ExponentialAnd(Expression):
    exponential_and_expr: Sequence[Expression]


@dataclass(frozen=True, init=False)
class Declaration(Node):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract type Declaration")


@dataclass(frozen=True)
class DefFunction(Declaration):
    declaration_name: Name
    declaration_body: Expression
