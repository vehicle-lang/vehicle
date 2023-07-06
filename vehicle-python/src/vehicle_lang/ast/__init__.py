import json
from abc import ABCMeta
from dataclasses import dataclass
from typing import Optional, Sequence

from typing_extensions import Self, TypeAlias, override

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
class Builtin(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Builtin")


@dataclass(frozen=True)
class AddInt(Builtin):
    pass


@dataclass(frozen=True)
class AddNat(Builtin):
    pass


@dataclass(frozen=True)
class AddRat(Builtin):
    pass


@dataclass(frozen=True)
class And(Builtin):
    pass


@dataclass(frozen=True)
class AtVector(Builtin):
    pass


@dataclass(frozen=True)
class Bool(Builtin):
    value: bool


@dataclass(frozen=True)
class BoolType(Builtin):
    pass


@dataclass(frozen=True)
class ConsList(Builtin):
    pass


@dataclass(frozen=True)
class ConsVector(Builtin):
    pass


@dataclass(frozen=True)
class DivRat(Builtin):
    pass


@dataclass(frozen=True)
class Eq(Builtin):
    pass


@dataclass(frozen=True)
class Exists(Builtin):
    pass


@dataclass(frozen=True)
class FoldList(Builtin):
    pass


@dataclass(frozen=True)
class FoldVector(Builtin):
    pass


@dataclass(frozen=True)
class Forall(Builtin):
    pass


@dataclass(frozen=True)
class GeIndex(Builtin):
    pass


@dataclass(frozen=True)
class GeInt(Builtin):
    pass


@dataclass(frozen=True)
class GeNat(Builtin):
    pass


@dataclass(frozen=True)
class GeRat(Builtin):
    pass


@dataclass(frozen=True)
class GtIndex(Builtin):
    pass


@dataclass(frozen=True)
class GtInt(Builtin):
    pass


@dataclass(frozen=True)
class GtNat(Builtin):
    pass


@dataclass(frozen=True)
class GtRat(Builtin):
    pass


@dataclass(frozen=True)
class If(Builtin):
    pass


@dataclass(frozen=True)
class Implies(Builtin):
    pass


@dataclass(frozen=True)
class Index(Builtin):
    value: int


@dataclass(frozen=True)
class IndexType(Builtin):
    pass


@dataclass(frozen=True)
class Indices(Builtin):
    pass


@dataclass(frozen=True)
class Int(Builtin):
    value: int


@dataclass(frozen=True)
class IntType(Builtin):
    pass


@dataclass(frozen=True)
class LeIndex(Builtin):
    pass


@dataclass(frozen=True)
class LeInt(Builtin):
    pass


@dataclass(frozen=True)
class LeNat(Builtin):
    pass


@dataclass(frozen=True)
class LeRat(Builtin):
    pass


@dataclass(frozen=True)
class ListType(Builtin):
    pass


@dataclass(frozen=True)
class LtIndex(Builtin):
    pass


@dataclass(frozen=True)
class LtInt(Builtin):
    pass


@dataclass(frozen=True)
class LtNat(Builtin):
    pass


@dataclass(frozen=True)
class LtRat(Builtin):
    pass


@dataclass(frozen=True)
class MaxRat(Builtin):
    pass


@dataclass(frozen=True)
class MinRat(Builtin):
    pass


@dataclass(frozen=True)
class MulInt(Builtin):
    pass


@dataclass(frozen=True)
class MulNat(Builtin):
    pass


@dataclass(frozen=True)
class MulRat(Builtin):
    pass


@dataclass(frozen=True)
class Nat(Builtin):
    value: int


@dataclass(frozen=True)
class NatType(Builtin):
    pass


@dataclass(frozen=True)
class Ne(Builtin):
    pass


@dataclass(frozen=True)
class NegInt(Builtin):
    pass


@dataclass(frozen=True)
class NegRat(Builtin):
    pass


@dataclass(frozen=True)
class NilList(Builtin):
    pass


@dataclass(frozen=True)
class Not(Builtin):
    pass


@dataclass(frozen=True)
class Or(Builtin):
    pass


@dataclass(frozen=True)
class PowRat(Builtin):
    pass


@dataclass(frozen=True)
class Rat(Builtin):
    numerator: int
    denominator: int


@dataclass(frozen=True)
class RatType(Builtin):
    pass


@dataclass(frozen=True)
class Sample(Builtin):
    name: Name
    locals: Sequence[Name]


@dataclass(frozen=True)
class SubInt(Builtin):
    pass


@dataclass(frozen=True)
class SubRat(Builtin):
    pass


@dataclass(frozen=True)
class Unit(Builtin):
    pass


@dataclass(frozen=True)
class UnitType(Builtin):
    pass


@dataclass(frozen=True)
class Vector(Builtin):
    value: int


@dataclass(frozen=True)
class VectorType(Builtin):
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
    provenance: Provenance
    func: Expression
    args: Sequence[Expression]


@dataclass(frozen=True)
class Binder(AST):
    provenance: Provenance
    name: Optional[Name]
    type: Expression


@dataclass(frozen=True)
class BoundVar(Expression):
    provenance: Provenance
    name: Name


@dataclass(frozen=True)
class BuiltinOp(Expression):
    provenance: Provenance
    builtin: Builtin


@dataclass(frozen=True)
class FreeVar(Expression):
    provenance: Provenance
    name: Name


@dataclass(frozen=True)
class Lam(Expression):
    provenance: Provenance
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Let(Expression):
    provenance: Provenance
    bound: Expression
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Pi(Expression):
    provenance: Provenance
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Universe(Expression):
    provenance: Provenance
    level: UniverseLevel


################################################################################
# Declarations
################################################################################


@dataclass(frozen=True, init=False)
class Declaration(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Declaration")


@dataclass(frozen=True)
class DefFunction(Declaration):
    provenance: Provenance
    name: Name
    type: Expression
    body: Expression


@dataclass(frozen=True)
class DefPostulate(Declaration):
    provenance: Provenance
    name: Name
    body: Expression


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
