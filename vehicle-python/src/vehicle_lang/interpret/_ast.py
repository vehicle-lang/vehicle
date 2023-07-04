import json
from abc import ABCMeta
from dataclasses import dataclass
from typing import Optional, Sequence

from typing_extensions import Literal, Self, TypeAlias, override

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
# Primitive
################################################################################


@dataclass(frozen=True, init=False)
class Builtin(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class Builtin")


@dataclass(frozen=True)
class Nil(Builtin):
    pass


@dataclass(frozen=True)
class Cons(Builtin):
    pass


@dataclass(frozen=True)
class Unit(Builtin):
    pass


@dataclass(frozen=True)
class Bool(Builtin):
    value: bool


@dataclass(frozen=True)
class Index(Builtin):
    value: int


@dataclass(frozen=True)
class Nat(Builtin):
    value: int


@dataclass(frozen=True)
class Int(Builtin):
    value: int


@dataclass(frozen=True)
class Rat(Builtin):
    numerator: int
    denominator: int


@dataclass(frozen=True)
class Vector(Builtin):
    value: int


@dataclass(frozen=True)
class Not(Builtin):
    pass


@dataclass(frozen=True)
class And(Builtin):
    pass


@dataclass(frozen=True)
class Or(Builtin):
    pass


@dataclass(frozen=True)
class Implies(Builtin):
    pass


@dataclass(frozen=True)
class Forall(Builtin):
    pass


@dataclass(frozen=True)
class Exists(Builtin):
    pass


@dataclass(frozen=True)
class If(Builtin):
    pass


@dataclass(frozen=True)
class Neg(Builtin):
    domain: Literal["Int", "Rat"]


@dataclass(frozen=True)
class Add(Builtin):
    domain: Literal["Nat", "Int", "Rat"]


@dataclass(frozen=True)
class Sub(Builtin):
    domain: Literal["Int", "Rat"]


@dataclass(frozen=True)
class Mul(Builtin):
    domain: Literal["Nat", "Int", "Rat"]


@dataclass(frozen=True)
class Div(Builtin):
    domain: Literal["Rat"]


@dataclass(frozen=True)
class Eq(Builtin):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Ne(Builtin):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Le(Builtin):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Lt(Builtin):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Ge(Builtin):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Gt(Builtin):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class At(Builtin):
    pass


@dataclass(frozen=True)
class ConsVector(Builtin):
    pass


@dataclass(frozen=True)
class Fold(Builtin):
    domain: Literal["List", "Vector"]


@dataclass(frozen=True)
class Indices(Builtin):
    pass


@dataclass(frozen=True)
class Min(Builtin):
    pass


@dataclass(frozen=True)
class Max(Builtin):
    pass


@dataclass(frozen=True)
class Power(Builtin):
    pass


@dataclass(frozen=True)
class Indicator(Builtin):
    pass


@dataclass(frozen=True)
class UnitType(Builtin):
    pass


@dataclass(frozen=True)
class BoolType(Builtin):
    pass


@dataclass(frozen=True)
class IndexType(Builtin):
    pass


@dataclass(frozen=True)
class NatType(Builtin):
    pass


@dataclass(frozen=True)
class IntType(Builtin):
    pass


@dataclass(frozen=True)
class RatType(Builtin):
    pass


@dataclass(frozen=True)
class ListType(Builtin):
    pass


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
class Binder(AST):
    provenance: Provenance
    name: Optional[Name]
    type: Expression


@dataclass(frozen=True)
class Universe(Expression):
    provenance: Provenance
    level: UniverseLevel


@dataclass(frozen=True)
class App(Expression):
    provenance: Provenance
    func: Expression
    args: Sequence[Expression]


@dataclass(frozen=True)
class Pi(Expression):
    provenance: Provenance
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class BuiltinOp(Expression):
    provenance: Provenance
    builtin: Builtin


@dataclass(frozen=True)
class BoundVar(Expression):
    provenance: Provenance
    name: Name


@dataclass(frozen=True)
class FreeVar(Expression):
    provenance: Provenance
    name: Name


@dataclass(frozen=True)
class Let(Expression):
    provenance: Provenance
    bound: Expression
    binder: Binder
    body: Expression


@dataclass(frozen=True)
class Lam(Expression):
    provenance: Provenance
    binder: Binder
    body: Expression


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
