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


################################################################################
# Builtin Constants
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinConstant(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class BuiltinConstant")


@dataclass(frozen=True)
class Unit(BuiltinConstant):
    pass


@dataclass(frozen=True)
class NilList(BuiltinConstant):
    pass


################################################################################
# Builtin Functions
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinFunction(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class BuiltinFunction")


@dataclass(frozen=True)
class ConsList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NotBoolTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class AndBoolTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class OrBoolTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NegRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class AddRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class SubRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MulRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class DivRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class EqRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NeRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LeRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LtRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GeRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GtRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class PowRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MinRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MaxRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ReduceAndBoolTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ReduceOrBoolTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ReduceSumRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ReduceRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class EqIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NeIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LeIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LtIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GeIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GtIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LookupRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class StackRatTensor(BuiltinFunction):
    value: int


@dataclass(frozen=True)
class ConstRatTensor(BuiltinFunction):
    value: Fraction


@dataclass(frozen=True)
class FoldList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MapList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MapRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ZipWithRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class IndicesIndexTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MinimiseRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MaximiseRatTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class If(BuiltinFunction):
    pass


################################################################################
# Builtin Literals
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinLiteral(AST):
    value: Any

    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class BuiltinLiteral")


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


################################################################################
# Builtin Types
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinType(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class BuiltinType")


@dataclass(frozen=True)
class IndexType(BuiltinType):
    pass


@dataclass(frozen=True)
class IndexTensorType(BuiltinType):
    pass


@dataclass(frozen=True)
class BoolTensorType(BuiltinType):
    pass


@dataclass(frozen=True)
class NatTensorType(BuiltinType):
    pass


@dataclass(frozen=True)
class IntTensorType(BuiltinType):
    pass


@dataclass(frozen=True)
class RatTensorType(BuiltinType):
    pass


@dataclass(frozen=True)
class ListType(BuiltinType):
    pass


@dataclass(frozen=True)
class UnitType(BuiltinType):
    pass


################################################################################
# Expressions
################################################################################


@dataclass(frozen=True, init=False)
class Expression(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Expression")


@dataclass(frozen=True)
class Binder(AST):
    provenance: Provenance = field(repr=False)
    name: Optional[Name]
    type: Expression


@dataclass(frozen=True)
class Pi(Expression):
    provenance: Provenance = field(repr=False)
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Lam(Expression):
    provenance: Provenance = field(repr=False)
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class App(Expression):
    provenance: Provenance = field(repr=False)
    function: Expression
    arguments: Sequence[Expression]


@dataclass(frozen=True)
class PartialApp(Expression):
    provenance: Provenance = field(repr=False)
    arity: int
    function: Expression
    arguments: Sequence[Expression]


@dataclass(frozen=True)
class Var(Expression):
    provenance: Provenance = field(repr=False)
    name: Name


@dataclass(frozen=True)
class Builtin(Expression):
    provenance: Provenance = field(repr=False)
    builtin: Union[BuiltinConstant, BuiltinFunction, BuiltinLiteral, BuiltinType]


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
