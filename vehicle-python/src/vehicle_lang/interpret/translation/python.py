import ast as py
from dataclasses import asdict, dataclass
from typing import Any, ClassVar, Dict, Sequence

from typing_extensions import override

from .._ast import (
    MISSING,
    Add,
    And,
    App,
    At,
    Binder,
    Bool,
    BoolType,
    BoundVar,
    Builtin,
    BuiltinOp,
    Cons,
    ConsVector,
    Declaration,
    DefFunction,
    DefPostulate,
    Div,
    Eq,
    Exists,
    Expression,
    Fold,
    Forall,
    FreeVar,
    Ge,
    Gt,
    If,
    Implies,
    Index,
    IndexType,
    Indicator,
    Indices,
    Int,
    IntType,
    Lam,
    Le,
    Let,
    ListType,
    Lt,
    Main,
    Max,
    Min,
    Mul,
    Nat,
    NatType,
    Ne,
    Neg,
    Nil,
    Not,
    Or,
    Pi,
    Pow,
    Program,
    Rat,
    RatType,
    Sub,
    Unit,
    UnitType,
    Universe,
    Vector,
    VectorType,
)
from ..builtin import BuiltinInterpreter
from . import ABCTranslation
from ._ast_compat import arguments as py_arguments
from ._ast_compat import dump as py_ast_dump
from ._ast_compat import unparse as py_ast_unparse


@dataclass(frozen=True)
class PythonTranslation(ABCTranslation[py.Module, py.stmt, py.expr, Any, py.expr]):
    builtinInterpreter: BuiltinInterpreter[
        Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any
    ]

    def translate_binder(self, binder: Binder) -> Any:
        return NotImplemented

    def translate_builtin(self, builtin: Builtin) -> py.expr:
        return py.Call(
            py.Attribute(
                py.Name("__builtin_interpreter__", py.Load(), **asdict(MISSING)),
                f"interpret_{type(builtin).__name__}",
                py.Load(),
                **asdict(MISSING),
            ),
            [],
            [],
            **asdict(MISSING),
        )

    def translate_Main(self, main: Main) -> py.Module:
        return NotImplemented

    def translate_DefPostulate(self, declaration: DefPostulate) -> py.stmt:
        return NotImplemented

    def translate_DefFunction(self, declaration: DefFunction) -> py.stmt:
        return NotImplemented

    def translate_App(self, expression: App) -> py.expr:
        return NotImplemented

    def translate_BoundVar(self, expression: BoundVar) -> py.expr:
        return NotImplemented

    def translate_BuiltinOp(self, expression: BuiltinOp) -> py.expr:
        return NotImplemented

    def translate_FreeVar(self, expression: FreeVar) -> py.expr:
        return NotImplemented

    def translate_Lam(self, expression: Lam) -> py.expr:
        return NotImplemented

    def translate_Let(self, expression: Let) -> py.expr:
        return NotImplemented

    def translate_Pi(self, expression: Pi) -> py.expr:
        return NotImplemented

    def translate_Universe(self, expression: Universe) -> py.expr:
        return NotImplemented
