import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from fractions import Fraction
from pathlib import Path
from typing import Any, Generic, Iterable, Optional, Sequence, Tuple, Union

from typing_extensions import Literal, Self, TypeAlias, TypeVar, override

from .. import session
from ..error import VehicleError
from ..typing import DeclarationName, Explicit, Target
from ._decode import JsonValue, decode

Name: TypeAlias = str
UniverseLevel: TypeAlias = int


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
# Provenance
################################################################################


@dataclass(frozen=True)
class Provenance(AST):
    lineno: int
    col_offset: int
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


MISSING: Provenance = Provenance(0, 0)

################################################################################
# Values
################################################################################


DType = TypeVar("DType", bool, float, int, Fraction)


@dataclass(frozen=True)
class Tensor(Generic[DType]):
    shape: Tuple[int, ...]
    value: Tuple[DType, ...]


#################################################################################
# Dimensions
#################################################################################


@dataclass(frozen=True)
class Dimension(AST):
    value: int


@dataclass(frozen=True)
class DimensionNil(AST):
    pass


@dataclass(frozen=True)
class DimensionCons(AST):
    head: Dimension
    tail: Union["DimensionCons", DimensionNil]


@dataclass(frozen=True)
class DimensionIndex(AST):
    value: int


################################################################################
# Abstract AST Nodes
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinLiteral(AST):
    # Abstract base for literal values
    value: Any

    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class")


@dataclass(frozen=True, init=False)
class BuiltinConstant(AST):
    # Abstract base for constant symbols
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class")


@dataclass(frozen=True, init=False)
class BuiltinType(AST):
    # Abstract base for built-in type symbols
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class")


@dataclass(frozen=True, init=False)
class BuiltinFunction(AST):
    # Abstract base for built-in function symbols
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class")


@dataclass(frozen=True, init=False)
class Expression(AST):
    # Abstract base for expression nodes
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class")


@dataclass(frozen=True, init=False)
class Declaration(AST, metaclass=ABCMeta):
    # Abstract base for top-level declarations
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class")


# Type Alias for common Union used in function arguments/bodies
# ==============================================================================

FunctionInput: TypeAlias = Union[
    BuiltinFunction, Expression, BuiltinConstant, BuiltinLiteral
]


################################################################################
# Builtin Literals
################################################################################


@dataclass(frozen=True)
class Index(BuiltinLiteral):
    value: int


@dataclass(frozen=True)
class BoolTensor(BuiltinLiteral):
    value: Tensor[bool]


@dataclass(frozen=True)
class NatTensor(BuiltinLiteral):
    value: Tensor[int]


@dataclass(frozen=True)
class IntTensor(BuiltinLiteral):
    value: Tensor[int]


@dataclass(frozen=True)
class RatTensor(BuiltinLiteral):
    value: Tensor[Fraction]


@dataclass(frozen=True)
class RatLiteral(BuiltinLiteral):
    value: Fraction


################################################################################
# Builtin Constants
################################################################################


@dataclass(frozen=True)
class Unit(BuiltinConstant):
    pass


@dataclass(frozen=True)
class ConstTensor(BuiltinConstant):
    literal: BuiltinLiteral
    dimension: DimensionCons


@dataclass(frozen=True)
class NilList(BuiltinConstant):
    pass


################################################################################
# Builtin Types
################################################################################


@dataclass(frozen=True)
class TensorType(BuiltinType):
    body: BuiltinType


@dataclass(frozen=True)
class IndexType(BuiltinType):
    pass


@dataclass(frozen=True)
class IndexTensorType(BuiltinType):
    pass


@dataclass(frozen=True)
class BoolType(BuiltinType):
    pass


@dataclass(frozen=True)
class NatType(BuiltinType):
    pass


@dataclass(frozen=True)
class IntType(BuiltinType):
    pass


@dataclass(frozen=True)
class RatType(BuiltinType):
    pass


@dataclass(frozen=True)
class ListType(BuiltinType):
    pass


@dataclass(frozen=True)
class UnitType(BuiltinType):
    pass


################################################################################
# Builtin Functions
################################################################################


@dataclass(frozen=True)
class ConsList(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class NotBoolTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class AndBoolTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class OrBoolTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class NegRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class AddRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class SubRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class MulRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class DivRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class EqRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class NeRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class LeRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class LtRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class GeRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class GtRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class PowRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class MinRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class MaxRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class ReduceAndBoolTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class ReduceOrBoolTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class ReduceSumRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class ReduceRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class ReduceMulRatTensor(BuiltinFunction):
    body: FunctionInput


@dataclass(frozen=True)
class EqIndex(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class NeIndex(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class LeIndex(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class LtIndex(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class GeIndex(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class GtIndex(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class LookupRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class StackTensor(BuiltinFunction):
    element_type: BuiltinType
    dimensions: Union[DimensionCons, DimensionNil]
    body: FunctionInput


@dataclass(frozen=True)
class ConstRatTensor(BuiltinFunction):
    body: Fraction


@dataclass(frozen=True)
class FoldList(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class MapList(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class MapRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class ZipWithRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class IndicesIndexTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class MinimiseRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class MaximiseRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class If(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class SearchRatTensor(BuiltinFunction):
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class DimensionLookup(BuiltinFunction):
    body: FunctionInput
    index: DimensionIndex


################################################################################
# Expressions
################################################################################


@dataclass(frozen=True)
class Binder(AST):
    provenance: Provenance = field(repr=False)
    name: Optional[Name]
    type: Union[Expression, BuiltinType]


@dataclass(frozen=True)
class Pi(Expression):
    body: Sequence[Union[Expression, BuiltinType]]


@dataclass(frozen=True)
class Lam(Expression):
    binder: Binder
    body: BuiltinFunction


@dataclass(frozen=True)
class App(Expression):
    provenance: Provenance = field(repr=False)
    body: Expression
    arguments: Sequence[Expression]


@dataclass(frozen=True)
class PartialApp(Expression):
    provenance: Provenance = field(repr=False)
    arity: int
    body: Expression
    arguments: Sequence[Expression]


@dataclass(frozen=True)
class Var(Expression):
    name: Name
    body: Sequence[FunctionInput]


@dataclass(frozen=True)
class Builtin(Expression):
    provenance: Provenance = field(repr=False)
    builtin: Union[BuiltinConstant, BuiltinFunction, BuiltinLiteral, BuiltinType]


################################################################################
# Declarations
################################################################################


@dataclass(frozen=True)
class DefFunction(Declaration):
    provenance: Provenance = field(repr=False)
    name: Name
    type: Expression
    body: Expression

    @override
    def get_name(self) -> Name:
        return self.name


@dataclass(frozen=True)
class DefPostulate(Declaration):
    provenance: Provenance = field(repr=False)
    name: Name
    body: Expression

    @override
    def get_name(self) -> Name:
        return self.name


################################################################################
# Modules
################################################################################


@dataclass(frozen=True, init=False)
class Program(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Program")

    @override
    @classmethod
    def from_dict(cls, value: JsonValue) -> Self:
        return decode(Program, value)


@dataclass(frozen=True)
class Main(Program):
    declarations: Sequence[Declaration]


def load(
    path: Union[str, Path],
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = Explicit.Explicit,
) -> Program:
    exc, out, err, log = session.check_output(
        [
            "compile",
            "--target",
            target._vehicle_option_name,
            "--json",
            f"--specification={path}",
            *[f"--declaration={declaration_name}" for declaration_name in declarations],
        ]
    )
    if exc != 0:
        msg: str = err or out or log or "unknown error"
        raise VehicleError(msg)
    if out is None:
        raise VehicleError("no output")
    return Program.from_json(out)
