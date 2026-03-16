import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from fractions import Fraction
from pathlib import Path
from typing import Optional, Sequence

from typing_extensions import Self, TypeAlias
from typing_extensions import TypeVar as TypingTypeVar
from typing_extensions import override

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
    end_lineno: Optional[int] = None
    end_col_offset: Optional[int] = None


MISSING: Provenance = Provenance(0, 0)

################################################################################
# Values
################################################################################


DType = TypingTypeVar("DType", bool, float, int, Fraction)


################################################################################
# Tensors
################################################################################


@dataclass(frozen=True)
class Tensor(AST):
    shape: Sequence[int]
    value: Sequence[Fraction] | Fraction

    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Tensor")


@dataclass(frozen=True)
class DenseTensor(Tensor):
    shape: Sequence[int]
    value: Sequence[Fraction]


@dataclass(frozen=True)
class ConstantTensor(Tensor):
    shape: Sequence[int]
    value: Fraction


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
class RatTensor(Expression):
    """RatTensor (Tensor Rat) from JSON"""

    contents: Tensor


@dataclass(frozen=True)
class NegRatTensor(Expression):
    """Unary negation: NegRatTensor expr"""

    x: Expression


@dataclass(frozen=True)
class AddRatTensor(Expression):
    """Binary addition: AddRatTensor left right - provides App interface for translation"""

    x: Expression
    y: Expression


@dataclass(frozen=True)
class SubRatTensor(Expression):
    """Binary subtraction: SubRatTensor left right - behaves like App for translation"""

    x: Expression
    y: Expression


@dataclass(frozen=True)
class MulRatTensor(Expression):
    """Binary multiplication: MulRatTensor left right"""

    x: Expression
    y: Expression


@dataclass(frozen=True)
class DivRatTensor(Expression):
    """Binary division: DivRatTensor left right"""

    x: Expression
    y: Expression


@dataclass(frozen=True)
class MinRatTensor(Expression):
    """Binary minimum: MinRatTensor left right"""

    x: Expression
    y: Expression


@dataclass(frozen=True)
class MaxRatTensor(Expression):
    """Binary maximum: MaxRatTensor left right"""

    x: Expression
    y: Expression


@dataclass(frozen=True)
class ReduceAddRatTensor(Expression):
    """Reduce addition: ReduceAddRatTensor expr dims"""

    f: Expression
    x: Expression


@dataclass(frozen=True)
class ReduceMulRatTensor(Expression):
    """Reduce multiplication: ReduceMulRatTensor expr dims"""

    f: Expression
    x: Expression


@dataclass(frozen=True)
class ReduceMinRatTensor(Expression):
    """Reduce minimum: ReduceMinRatTensor expr dims"""

    f: Expression
    x: Expression


@dataclass(frozen=True)
class ReduceMaxRatTensor(Expression):
    """Reduce maximum: ReduceMaxRatTensor expr dims"""

    f: Expression
    x: Expression


@dataclass(frozen=True)
class SearchRatTensor(Expression):
    """Search tensor: SearchRatTensor reductionOp lowerBound upperBound searchLambda"""

    name: str
    reduction_op: Expression
    dims: Expression
    lower_bound: Expression
    upper_bound: Expression
    search_lambda: Expression
    minimise: bool


@dataclass(frozen=True)
class Dimension(Expression):
    """Dimension Int - for JSON parsing"""

    value: int


@dataclass(frozen=True)
class DimensionNil(Expression):
    """DimensionNil - for JSON parsing"""


@dataclass(frozen=True)
class DimensionLookup(Expression):
    """Dimension lookup: DimensionLookup tensor index"""

    xs: Expression
    i: Expression


@dataclass(frozen=True)
class DimensionCons(Expression):
    e1: Expression
    e2: Expression


@dataclass(frozen=True)
class DimensionIndex(Expression):
    i: int


@dataclass(frozen=True)
class ConstTensor(Expression):
    """ConstTensor shape value - for JSON parsing"""

    c: Fraction
    ds: Sequence[int]


@dataclass(frozen=True)
class StackTensor(Expression):
    """StackTensor : StackTensor tensor_list"""

    xs: Sequence[Expression]


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
    type: BuiltinType
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
