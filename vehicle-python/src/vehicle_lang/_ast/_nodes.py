import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from fractions import Fraction
from pathlib import Path
from typing import Generic, Literal, Optional, Sequence

from typing_extensions import Self, TypeAlias
from typing_extensions import TypeVar as TypingTypeVar
from typing_extensions import override

from ._decode import JsonValue, decode

Name: TypeAlias = str
UniverseLevel: TypeAlias = int
ComparisonOp: TypeAlias = Literal["Eq", "Ne", "Le", "Lt", "Ge", "Gt"]


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

    @classmethod
    def from_file(cls, path: str | Path) -> Self:
        with open(path, "r", encoding="utf-8") as file:
            return cls.from_json(file.read())


################################################################################
# Provenance
################################################################################


@dataclass(frozen=True)
class Provenance(AST):
    lineno: int
    col_offset: int
    end_lineno: int
    end_col_offset: int

    def __str__(self) -> str:
        if self.lineno == self.end_lineno:
            return f"Line {self.lineno}:{self.col_offset}-{self.end_col_offset}"
        return f"Lines {self.lineno}:{self.col_offset}-{self.end_lineno}:{self.end_col_offset}"


MISSING: Provenance = Provenance(0, 0, 0, 0)


################################################################################
# Extended rationals
################################################################################


@dataclass(frozen=True, init=False)
class ExtendedFraction(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class ExtendedFraction")


@dataclass(frozen=True)
class Finite(ExtendedFraction):
    value: Fraction


@dataclass(frozen=True)
class PosInfinity(ExtendedFraction):
    pass


@dataclass(frozen=True)
class NegInfinity(ExtendedFraction):
    pass


################################################################################
# Values
################################################################################


DType = TypingTypeVar("DType", bool, float, int, ExtendedFraction)


################################################################################
# Tensors
################################################################################


@dataclass(frozen=True)
class Tensor(AST, Generic[DType]):
    shape: Sequence[int]

    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Tensor")


@dataclass(frozen=True)
class DenseTensor(Tensor[DType], Generic[DType]):
    shape: Sequence[int]
    values: Sequence[DType]


@dataclass(frozen=True)
class ConstantTensor(Tensor[DType], Generic[DType]):
    shape: Sequence[int]
    value: DType


################################################################################
# Builtin Types
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinType(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class BuiltinType")


@dataclass(frozen=True)
class Pi(BuiltinType):
    """Pi type: Pi input_type output_type"""

    input_type: BuiltinType
    output_type: BuiltinType


@dataclass(frozen=True)
class RatType(BuiltinType):
    """Rational number type: RatType"""


@dataclass(frozen=True)
class BoolType(BuiltinType):
    """Boolean type: BoolType"""


@dataclass(frozen=True)
class VectorType(BuiltinType):
    """Vector type: VectorType base_type"""

    base_type: BuiltinType


@dataclass(frozen=True)
class TensorType(BuiltinType):
    """Tensor type: TensorType base_type"""

    base_type: BuiltinType


@dataclass(frozen=True)
class DimensionType(BuiltinType):
    """Dimension type: DimensionType"""


@dataclass(frozen=True)
class DimensionsType(BuiltinType):
    """Dimensions type: DimensionsType"""


@dataclass(frozen=True)
class DimensionIndexType(BuiltinType):
    """DimensionIndex type: DimensionIndexType"""


@dataclass(frozen=True)
class TypeVar(BuiltinType):
    """Type variable: TypeVar name arguments"""

    name: str
    spine: Sequence[BuiltinType]


################################################################################
# Expressions
################################################################################


@dataclass(frozen=True)
class Binder(AST):
    provenance: Provenance = field(repr=False)
    name: Optional[Name]
    type: BuiltinType


@dataclass(frozen=True, init=False)
class Expression(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Expression")


@dataclass(frozen=True)
class Lam(Expression):
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class App(Expression):
    provenance: Provenance = field(repr=False)
    function: Expression
    arguments: Sequence[Expression]


@dataclass(frozen=True)
class Var(Expression):
    name: str
    arguments: Sequence[Expression]


@dataclass(frozen=True)
class Let(Expression):
    bound: Expression
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Record(Expression):
    fields: Sequence[tuple[Name, Expression]]


@dataclass(frozen=True)
class RecordAcc(Expression):
    record: Expression
    field: Name
    arguments: Sequence[Expression]


@dataclass(frozen=True)
class BoolTensor(Expression):
    contents: Tensor[bool]


@dataclass(frozen=True)
class BoolNot(Expression):
    x: Expression


@dataclass(frozen=True)
class BoolAnd(Expression):
    x: Expression
    y: Expression


@dataclass(frozen=True)
class BoolOr(Expression):
    x: Expression
    y: Expression


@dataclass(frozen=True)
class BoolImplies(Expression):
    x: Expression
    y: Expression


@dataclass(frozen=True)
class BoolCompareIndex(Expression):
    op: ComparisonOp
    x: Expression
    y: Expression


@dataclass(frozen=True)
class BoolCompareNat(Expression):
    op: ComparisonOp
    x: Expression
    y: Expression


@dataclass(frozen=True)
class BoolCompareRatTensor(Expression):
    op: ComparisonOp
    p_dims: Expression
    r_dims: Expression
    x: Expression
    y: Expression


@dataclass(frozen=True)
class BoolReduceAnd(Expression):
    x: Expression


@dataclass(frozen=True)
class BoolReduceOr(Expression):
    x: Expression


@dataclass(frozen=True)
class BoolIf(Expression):
    c: Expression
    x: Expression
    y: Expression


@dataclass(frozen=True)
class RatTensor(Expression):
    contents: Tensor[ExtendedFraction]


@dataclass(frozen=True)
class NegRatTensor(Expression):
    """Unary negation: NegRatTensor expr"""

    x: Expression


@dataclass(frozen=True)
class AddRatTensor(Expression):

    x: Expression
    y: Expression


@dataclass(frozen=True)
class SubRatTensor(Expression):

    x: Expression
    y: Expression


@dataclass(frozen=True)
class MulRatTensor(Expression):

    x: Expression
    y: Expression


@dataclass(frozen=True)
class DivRatTensor(Expression):

    x: Expression
    y: Expression


@dataclass(frozen=True)
class MinRatTensor(Expression):

    x: Expression
    y: Expression


@dataclass(frozen=True)
class MaxRatTensor(Expression):

    x: Expression
    y: Expression


@dataclass(frozen=True)
class PowRatTensor(Expression):

    x: Expression
    y: Expression


@dataclass(frozen=True)
class LogRatTensor(Expression):

    x: Expression


@dataclass(frozen=True)
class ExpRatTensor(Expression):

    x: Expression


@dataclass(frozen=True)
class ReduceAddRatTensor(Expression):

    x: Expression


@dataclass(frozen=True)
class ReduceMulRatTensor(Expression):

    x: Expression


@dataclass(frozen=True)
class ReduceMinRatTensor(Expression):

    x: Expression


@dataclass(frozen=True)
class ReduceMaxRatTensor(Expression):

    x: Expression


@dataclass(frozen=True)
class SearchRatTensor(Expression):

    name: str
    dims: Expression
    lower_bound: Expression
    upper_bound: Expression
    search_lambda: Lam


@dataclass(frozen=True)
class WhereTensor(Expression):

    input_tensor: Expression
    condition: Expression
    false_value: Expression


@dataclass(frozen=True)
class Transpose(Expression):
    """Transpose: reverses all axes of a tensor (numpy-style)."""

    xs: Expression


@dataclass(frozen=True)
class Dimension(Expression):

    value: int


@dataclass(frozen=True)
class DimensionNil(Expression):
    pass


@dataclass(frozen=True)
class DimensionCons(Expression):
    e1: Expression
    e2: Expression


@dataclass(frozen=True)
class DimensionIndex(Expression):
    i: int


@dataclass(frozen=True)
class ConstTensor(Expression):

    c: Expression
    ds: Expression


@dataclass(frozen=True)
class StackTensor(Expression):

    xs: Sequence[Expression]


@dataclass(frozen=True)
class AtTensor(Expression):
    """At tensor: AtTensor tensor index"""

    xs: Expression
    i: Expression


@dataclass(frozen=True)
class ForeachTensor(Expression):
    """Foreach tensor: ForeachTensor size function"""

    size: Expression
    function: Expression


@dataclass(frozen=True)
class VectorLiteral(Expression):
    elements: Sequence[Expression]


@dataclass(frozen=True)
class AtVector(Expression):
    """At vector: AtVector vector index"""

    xs: Expression
    i: Expression


@dataclass(frozen=True)
class ForeachVector(Expression):
    """Foreach vector: ForeachVector size function"""

    size: Expression
    function: Expression


################################################################################
# Declarations
################################################################################


@dataclass(frozen=True, init=False)
class Declaration(AST, metaclass=ABCMeta):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Declaration")

    @abstractmethod
    def get_name(self) -> Name: ...


@dataclass(frozen=True)
class DefFunction(Declaration):
    provenance: Provenance = field(repr=False)
    name: Name
    isProperty: bool
    type: BuiltinType
    body: Expression

    @override
    def get_name(self) -> Name:
        return self.name


@dataclass(frozen=True)
class DefAbstract(Declaration):
    provenance: Provenance = field(repr=False)
    name: Name
    sort: str
    type: BuiltinType

    @override
    def get_name(self) -> Name:
        return self.name


################################################################################
# Boolean Tree
################################################################################


@dataclass(frozen=True)
class BooleanExpression(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class BooleanExpression")


@dataclass(frozen=True)
class Conjunct(BooleanExpression):
    conjunct_all: Sequence[BooleanExpression]


@dataclass(frozen=True)
class Disjunct(BooleanExpression):
    disjunct_all: Sequence[BooleanExpression]


@dataclass(frozen=True)
class Query(BooleanExpression):
    negated: bool
    disjunct_all: Sequence[Name]


@dataclass(frozen=True)
class BooleanTree(AST):
    provenance: Provenance = field(repr=False)
    name: Name
    boolean_expression: BooleanExpression


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
        return decode(cls, value)


@dataclass(frozen=True)
class Main(Program):
    declarations: Sequence[Declaration]


@dataclass(frozen=True)
class SearchMain(Program):
    """Stores boolean trees and declarations"""

    trees: Sequence[BooleanTree]
    program: Main
