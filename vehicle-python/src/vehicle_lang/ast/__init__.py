import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable, Optional, Sequence, Union

from typing_extensions import Self, TypeAlias, override

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
# Builtins
################################################################################


@dataclass(frozen=True, init=False)
class BuiltinFunction(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Builtin")


@dataclass(frozen=True)
class AddInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class AddNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class AddRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class And(BuiltinFunction):
    pass


@dataclass(frozen=True)
class AtVector(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Bool(BuiltinFunction):
    value: bool


@dataclass(frozen=True)
class BoolType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ConsList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class DivRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class EqIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class EqInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class EqNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class EqRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Exists(BuiltinFunction):
    pass


@dataclass(frozen=True)
class FoldList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class FoldVector(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Forall(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GeIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GeInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GeNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GeRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GtIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GtInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GtNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class GtRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class If(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Implies(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Index(BuiltinFunction):
    value: int


@dataclass(frozen=True)
class IndexType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Indices(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Int(BuiltinFunction):
    value: int


@dataclass(frozen=True)
class IntType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LeIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LeInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LeNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LeRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ListType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LtIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LtInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LtNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class LtRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MapList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MapVector(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MaxRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MinRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MulInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MulNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class MulRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Nat(BuiltinFunction):
    value: int


@dataclass(frozen=True)
class NatType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NeIndex(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NeInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NeNat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NeRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NegInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NegRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class NilList(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Not(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Optimise(BuiltinFunction):
    minimise: bool
    context: Sequence[Name]


@dataclass(frozen=True)
class Or(BuiltinFunction):
    pass


@dataclass(frozen=True)
class PowRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Rat(BuiltinFunction):
    numerator: int
    denominator: int


@dataclass(frozen=True)
class RatType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class SubInt(BuiltinFunction):
    pass


@dataclass(frozen=True)
class SubRat(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Unit(BuiltinFunction):
    pass


@dataclass(frozen=True)
class UnitType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class Vector(BuiltinFunction):
    value: int


@dataclass(frozen=True)
class VectorType(BuiltinFunction):
    pass


@dataclass(frozen=True)
class ZipWithVector(BuiltinFunction):
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
    def get_name(self) -> Name:
        ...


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
