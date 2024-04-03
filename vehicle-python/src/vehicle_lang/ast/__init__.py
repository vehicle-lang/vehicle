import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from pathlib import Path
from typing import Generic, Iterable, Optional, Sequence, Tuple, Union

from typing_extensions import Self, TypeAlias, TypeVar, override

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


@dataclass(frozen=True)
class Rational:
    pass


_DType = TypeVar("_DType", bool, float, int, Rational)


@dataclass(frozen=True)
class Tensor(Generic[_DType]):
    shape: Tuple[int, ...]
    value: Tuple[_DType]


################################################################################
# Builtins
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinFunction(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Builtin")


@dataclass(frozen=True)
class IndexType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class BoolTensorType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class IndexTensorType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NatTensorType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class IntTensorType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class RatTensorType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ListType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Unit(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Index(BuiltinFunction):
    value: int


@dataclass(frozen=True)
class BoolTensor(BuiltinFunction):
    value: Tensor[bool]


@dataclass(frozen=True)
class NatTensor(BuiltinFunction):
    value: Tensor[int]


@dataclass(frozen=True)
class IntTensor(BuiltinFunction):
    value: Tensor[int]


@dataclass(frozen=True)
class RatTensor(BuiltinFunction):
    value: Tensor[Rational]


@dataclass(frozen=True)
class NilList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ConsList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NotTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class AndTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class OrTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NegTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class AddTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class SubTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MulTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class DivTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class EqTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NeTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LeTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LtTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GeTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GtTensor(BuiltinFunction):
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
class ReduceAndTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ReduceOrTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ReduceSumTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ReduceTensor(BuiltinFunction):
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
class LookupTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class StackTensor(BuiltinFunction):
    value: int


@dataclass(frozen=True)
class ConstTensor(BuiltinFunction):
    value: Rational


@dataclass(frozen=True)
class FoldList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MapList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MapTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ZipWithTensor(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Indices(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Optimise(BuiltinFunction):
    minimise: bool


@dataclass(frozen=True)
class If(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Forall(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Exists(BuiltinFunction):
    pass


################################################################################
# Expressions
################################################################################


@dataclass(frozen=True, init=False)
class Expression(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Expression")


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
class Binder(AST):
    provenance: Provenance = field(repr=False)
    name: Optional[Name]
    type: Expression


@dataclass(frozen=True)
class BoundVar(Expression):
    provenance: Provenance = field(repr=False)
    name: Name


@dataclass(frozen=True)
class Builtin(Expression):
    provenance: Provenance = field(repr=False)
    builtin: BuiltinFunction


@dataclass(frozen=True)
class FreeVar(Expression):
    provenance: Provenance = field(repr=False)
    name: Name


@dataclass(frozen=True)
class Lam(Expression):
    provenance: Provenance = field(repr=False)
    binders: Sequence[Binder]
    body: Expression


@dataclass(frozen=True)
class Let(Expression):
    provenance: Provenance = field(repr=False)
    bound: Expression
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Pi(Expression):
    provenance: Provenance = field(repr=False)
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Universe(Expression):
    provenance: Provenance = field(repr=False)
    level: UniverseLevel


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
