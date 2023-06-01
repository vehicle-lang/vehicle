import dataclasses
import json
from abc import ABCMeta
from dataclasses import asdict, dataclass
from typing import Dict, Optional, Sequence, Tuple, Union

from typing_extensions import (
    Literal,
    Protocol,
    Self,
    TypeAlias,
    TypedDict,
    runtime_checkable,
)

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


class Provenance(TypedDict):
    lineno: int
    col_offset: int
    end_lineno: Optional[int]
    end_col_offset: Optional[int]


class HasProvenance(Protocol):
    lineno: int
    col_offset: int
    end_lineno: Optional[int]
    end_col_offset: Optional[int]


def provenance(node: HasProvenance) -> Provenance:
    return {
        "lineno": node.lineno,
        "col_offset": node.col_offset,
        "end_lineno": node.end_lineno,
        "end_col_offset": node.end_col_offset,
    }


@dataclass
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


@dataclass(init=False)
class Expression(Node):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract type Expression")


@dataclass
class UnaryOperator(Expression):
    unary_operator_name: UnaryOperatorName
    unary_operator_arg: Expression

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class BinaryOperator(Expression):
    binary_operator_name: BinaryOperatorName
    binary_operator_arg1: Expression
    binary_operator_arg2: Expression

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class Constant(Expression):
    constant_value: Union[int, float, complex]

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class Variable(Expression):
    variable_name: Name

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class FreeVariable(Expression):
    function_name: Name
    function_args: Sequence[Expression]

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class NetworkApplication(Expression):
    network_name: Name
    network_args: Sequence[Expression]

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class Quantifier(Expression):
    quantifier: QuantifierName
    quantifier_bound_name: Name
    quantifier_domain: DomainType
    quantifier_body: Expression

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class TensorLiteral(Expression):
    sequence: Sequence[Expression]

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class Let(Expression):
    let_bound_name: Name
    let_bound_expr: Expression
    let_body: Expression

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class Lambda(Expression):
    lambda_bound_name: Name
    lambda_body: Expression

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class Range(Expression):
    range_expr: Expression

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class ExponentialAnd(Expression):
    exponential_and_expr: Sequence[Expression]

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass(init=False)
class Declaration(Node):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract type Declaration")


@dataclass
class DefFunction(Declaration):
    declaration_name: Name
    declaration_body: Expression

    lineno: int = 0
    col_offset: int = 0
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


@dataclass
class Module(Node):
    declarations: Sequence[Declaration]
