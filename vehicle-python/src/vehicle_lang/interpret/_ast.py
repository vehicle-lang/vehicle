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
class BuiltinOp(AST):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class BuiltinOp")


@dataclass(frozen=True)
class Nil(BuiltinOp):
    pass


@dataclass(frozen=True)
class Cons(BuiltinOp):
    pass


@dataclass(frozen=True)
class LUnit(BuiltinOp):
    pass


@dataclass(frozen=True)
class LBool(BuiltinOp):
    value: bool


@dataclass(frozen=True)
class LIndex(BuiltinOp):
    value: int


@dataclass(frozen=True)
class LNat(BuiltinOp):
    value: int


@dataclass(frozen=True)
class LInt(BuiltinOp):
    value: int


@dataclass(frozen=True)
class LRat(BuiltinOp):
    numerator: int
    denominator: int


@dataclass(frozen=True)
class LVec(BuiltinOp):
    value: int


@dataclass(frozen=True)
class Not(BuiltinOp):
    pass


@dataclass(frozen=True)
class And(BuiltinOp):
    pass


@dataclass(frozen=True)
class Or(BuiltinOp):
    pass


@dataclass(frozen=True)
class Implies(BuiltinOp):
    pass


@dataclass(frozen=True)
class Forall(BuiltinOp):
    pass


@dataclass(frozen=True)
class Exists(BuiltinOp):
    pass


@dataclass(frozen=True)
class If(BuiltinOp):
    pass


@dataclass(frozen=True)
class Neg(BuiltinOp):
    domain: Literal["Int", "Rat"]


@dataclass(frozen=True)
class Add(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat"]


@dataclass(frozen=True)
class Sub(BuiltinOp):
    domain: Literal["Int", "Rat"]


@dataclass(frozen=True)
class Mul(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat"]


@dataclass(frozen=True)
class Div(BuiltinOp):
    domain: Literal["Rat"]


@dataclass(frozen=True)
class Eq(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Ne(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Le(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Lt(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Ge(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class Gt(BuiltinOp):
    domain: Literal["Nat", "Int", "Rat", "Index"]


@dataclass(frozen=True)
class At(BuiltinOp):
    pass


@dataclass(frozen=True)
class ConsVector(BuiltinOp):
    pass


@dataclass(frozen=True)
class Fold(BuiltinOp):
    domain: Literal["List", "Vector"]


@dataclass(frozen=True)
class Indices(BuiltinOp):
    pass


@dataclass(frozen=True)
class Min(BuiltinOp):
    pass


@dataclass(frozen=True)
class Max(BuiltinOp):
    pass


@dataclass(frozen=True)
class Power(BuiltinOp):
    pass


@dataclass(frozen=True)
class Indicator(BuiltinOp):
    pass


@dataclass(frozen=True)
class Unit(BuiltinOp):
    pass


@dataclass(frozen=True)
class Bool(BuiltinOp):
    pass


@dataclass(frozen=True)
class Index(BuiltinOp):
    pass


@dataclass(frozen=True)
class Nat(BuiltinOp):
    pass


@dataclass(frozen=True)
class Int(BuiltinOp):
    pass


@dataclass(frozen=True)
class Rat(BuiltinOp):
    pass


@dataclass(frozen=True)
class List(BuiltinOp):
    pass


@dataclass(frozen=True)
class Vector(BuiltinOp):
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
class Builtin(Expression):
    provenance: Provenance
    builtin: BuiltinOp


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
