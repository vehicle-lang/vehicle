import ast as py
import operator
from dataclasses import asdict, dataclass
from functools import partial, reduce
from pathlib import Path
from typing import Any, ClassVar, Dict, List, Optional, Sequence, Type, Union

from typing_extensions import TypeVar, override

from .. import session as global_session
from ..session import Session
from . import _ast as vcl
from ._ast_compat import arguments as py_arguments
from ._ast_compat import dump as py_ast_dump
from ._ast_compat import unparse as py_ast_unparse
from ._functools import (
    Function1,
    Function2,
    Function3,
    Operator1,
    Operator2,
    Relation2,
    curry,
)
from ._target import Target
from .abc import ABCTranslation, Builtins

_T = TypeVar("_T")

################################################################################
### Implementation of Vehicle builtins in Python
################################################################################


class PythonBuiltins(
    Builtins[
        bool,
        int,
        int,
        int,
        float,
    ],
):
    @override
    def AddInt(self) -> Operator2[int]:
        return curry(operator.add)

    @override
    def AddNat(self) -> Operator2[int]:
        return curry(operator.add)

    @override
    def AddRat(self) -> Operator2[float]:
        return curry(operator.add)

    @override
    def And(self) -> Operator2[bool]:
        return curry(operator.and_)

    @override
    def AtVector(self) -> Function2[Sequence[_T], int, _T]:
        return curry(operator.getitem)

    @override
    def Bool(self, value: bool) -> bool:
        return value

    @override
    def BoolType(self) -> Type[bool]:
        return bool

    @override
    def DivRat(self) -> Operator2[float]:
        return curry(operator.truediv)

    @override
    def Eq(self) -> Relation2[_T, bool]:
        return curry(operator.eq)

    @override
    def Exists(self) -> Function1[Function1[_T, bool], bool]:
        return NotImplemented

    @override
    def Forall(self) -> Function1[Function1[_T, bool], bool]:
        return NotImplemented

    @override
    def GeIndex(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def GeInt(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def GeNat(self) -> Relation2[int, bool]:
        return curry(operator.ge)

    @override
    def GeRat(self) -> Relation2[float, bool]:
        return curry(operator.ge)

    @override
    def GtIndex(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def GtInt(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def GtNat(self) -> Relation2[int, bool]:
        return curry(operator.gt)

    @override
    def GtRat(self) -> Relation2[float, bool]:
        return curry(operator.gt)

    @override
    def If(self) -> Function3[bool, _T, _T, _T]:
        return lambda i: lambda t: lambda e: t if i else e

    @override
    def Implies(self) -> Operator2[bool]:
        return lambda x: lambda y: (not x) or y

    @override
    def Index(self, value: int) -> int:
        return value

    @override
    def IndexType(self) -> Type[int]:
        return int

    @override
    def Indices(self) -> Function1[int, Sequence[int]]:
        return partial(range, 0)

    @override
    def Int(self, value: int) -> int:
        return value

    @override
    def IntType(self) -> Type[int]:
        return int

    @override
    def LeIndex(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def LeInt(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def LeNat(self) -> Relation2[int, bool]:
        return curry(operator.le)

    @override
    def LeRat(self) -> Relation2[float, bool]:
        return curry(operator.le)

    @override
    def LtIndex(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def LtInt(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def LtNat(self) -> Relation2[int, bool]:
        return curry(operator.lt)

    @override
    def LtRat(self) -> Relation2[float, bool]:
        return curry(operator.lt)

    @override
    def MaxRat(self) -> Operator2[float]:
        return curry(max)

    @override
    def MinRat(self) -> Operator2[float]:
        return curry(min)

    @override
    def MulInt(self) -> Operator2[int]:
        return curry(operator.mul)

    @override
    def MulNat(self) -> Operator2[int]:
        return curry(operator.mul)

    @override
    def MulRat(self) -> Operator2[float]:
        return curry(operator.mul)

    @override
    def Nat(self, value: int) -> int:
        return value

    @override
    def NatType(self) -> Type[int]:
        return int

    @override
    def Ne(self) -> Relation2[_T, bool]:
        return curry(operator.ne)

    @override
    def NegInt(self) -> Operator1[int]:
        return operator.neg

    @override
    def NegRat(self) -> Operator1[float]:
        return operator.neg

    @override
    def Not(self) -> Operator1[bool]:
        return operator.not_

    @override
    def Or(self) -> Operator2[bool]:
        return curry(operator.or_)

    @override
    def PowRat(self) -> Operator2[float]:
        return curry(operator.pow)

    @override
    def Rat(self, numerator: int, denominator: int) -> float:
        return numerator / denominator

    @override
    def RatType(self) -> Type[float]:
        return float

    @override
    def SubInt(self) -> Operator2[int]:
        return curry(operator.sub)

    @override
    def SubRat(self) -> Operator2[float]:
        return curry(operator.sub)


################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


@dataclass(frozen=True)
class PythonTranslation(ABCTranslation[py.Module, py.stmt, py.expr]):
    builtins: Builtins[Any, Any, Any, Any, Any]
    module_header: ClassVar[Sequence[py.stmt]] = []
    module_footer: ClassVar[Sequence[py.stmt]] = []

    def compile(
        self,
        program: vcl.Program,
        filename: str,
        declaration_context: Dict[str, Any] = {},
    ) -> Dict[str, Any]:
        py_ast = self.translate_program(program)
        try:
            declaration_context["__vehicle__"] = self.builtins
            py_bytecode = compile(py_ast, filename=filename, mode="exec")
            exec(py_bytecode, declaration_context)
            return declaration_context
        except TypeError as e:
            py_ast_str: str
            try:
                py_ast_str = py_ast_unparse(py_ast)
            except:
                py_ast_str = py_ast_dump(py_ast)
            raise TypeError(f"{e}:\n{py_ast_str}")

    def translate_Main(self, program: vcl.Main) -> py.Module:
        return py.Module(
            body=[
                *self.__class__.module_header,
                *map(self.translate_declaration, program.declarations),
                *self.__class__.module_footer,
            ],
            type_ignores=[],
        )

    def translate_binder(self, binder: vcl.Binder) -> py.arguments:
        return py_binder(
            py.arg(
                arg=binder.name or "_",
                annotation=None,
                **asdict(binder.provenance),
            )
        )

    def translate_DefPostulate(self, declaration: vcl.DefPostulate) -> py.stmt:
        return py.Assert(
            test=py.Compare(
                left=py.Str(
                    s=declaration.name,
                    **asdict(declaration.provenance),
                ),
                ops=[py.In()],
                comparators=[
                    py.Call(
                        func=py.Name(
                            id="vars",
                            ctx=py.Load(),
                            **asdict(declaration.provenance),
                        ),
                        args=[],
                        keywords=[],
                        **asdict(declaration.provenance),
                    )
                ],
                **asdict(declaration.provenance),
            ),
            msg=py.Str(
                s=f"The postulate {declaration.name} is undefined",
                **asdict(declaration.provenance),
            ),
            **asdict(declaration.provenance),
        )

    def translate_DefFunction(self, declaration: vcl.DefFunction) -> py.stmt:
        return py.Assign(
            targets=[
                py.Name(
                    id=declaration.name,
                    ctx=py.Store(),
                    **asdict(declaration.provenance),
                )
            ],
            value=self.translate_expression(declaration.body),
            **asdict(declaration.provenance),
        )

    def translate_App(self, expression: vcl.App) -> py.expr:
        # NOTE: Vector literals are handled separately:
        #       Vehicle represents Vector literals as N-ary functions,
        #       but (1) we cannot easily represent these in mypy,
        #       and (2) we risk exceeding the maximum recursion depth
        #       if we constructor vectors with recursive function calls.
        #       We intercept Vector literals and pass the list of arguments
        #       directly to the Vector builtin.
        if (
            isinstance(expression.func, vcl.BuiltinOp)
            and isinstance(expression.func.builtin, vcl.Vector)
            and expression.func.builtin.value > 0
        ):
            return py_builtin(
                "Vector",
                [
                    py.keyword(
                        arg="values",
                        value=py.Tuple(
                            elts=[
                                self.translate_expression(arg)
                                for arg in expression.args
                            ],
                            ctx=py.Load(),
                            **asdict(expression.provenance),
                        ),
                        **asdict(expression.provenance),
                    )
                ],
                provenance=expression.provenance,
            )
        else:
            return reduce(
                partial(py_app, provenance=expression.provenance),
                [self.translate_expression(arg) for arg in expression.args],
                self.translate_expression(expression.func),
            )

    def translate_BoundVar(self, expression: vcl.BoundVar) -> py.expr:
        return py.Name(
            id=expression.name,
            ctx=py.Load(),
            **asdict(expression.provenance),
        )

    def translate_BuiltinOp(self, expression: vcl.BuiltinOp) -> py.expr:
        keywords: List[py.keyword] = []
        if isinstance(expression.builtin, vcl.Bool):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.NameConstant(
                        value=True,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Index):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Int):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Nat):
            keywords.append(
                py.keyword(
                    arg="value",
                    value=py.Num(
                        n=expression.builtin.value,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Rat):
            keywords.append(
                py.keyword(
                    arg="numerator",
                    value=py.Num(
                        n=expression.builtin.numerator,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
            keywords.append(
                py.keyword(
                    arg="denominator",
                    value=py.Num(
                        n=expression.builtin.denominator,
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Vector):
            # NOTE: Vector literals are handled separately, as Vehicle represents
            #       these as N-ary functions. The only vectors that should be found
            #       at this point are empty vectors.
            assert (
                expression.builtin.value == 0
            ), "Found non-empty vector. These should be handled at their corresponding App node."
        return py_builtin(
            builtin=expression.builtin.__class__.__name__,
            keywords=keywords,
            provenance=expression.provenance,
        )

    def translate_FreeVar(self, expression: vcl.FreeVar) -> py.expr:
        return py.Name(
            id=expression.name,
            ctx=py.Load(),
            **asdict(expression.provenance),
        )

    def translate_Lam(self, expression: vcl.Lam) -> py.expr:
        return py.Lambda(
            args=self.translate_binder(expression.binder),
            body=self.translate_expression(expression.body),
            **asdict(expression.provenance),
        )

    def translate_Pi(self, _expression: vcl.Pi) -> py.expr:
        return NotImplemented

    def translate_Universe(self, _expression: vcl.Universe) -> py.expr:
        return NotImplemented


def py_binder(*args: py.arg) -> py.arguments:
    """Make a binder which only uses args."""
    return py_arguments(
        posonlyargs=[],
        args=list(args),
        vararg=None,
        kwonlyargs=[],
        kw_defaults=[],
        kwarg=None,
        defaults=[],
    )


def py_app(func: py.expr, arg: py.arg, *, provenance: vcl.Provenance) -> py.expr:
    """Make a function call."""
    return py.Call(
        func=func,
        args=[arg],
        keywords=[],
        **asdict(provenance),
    )


def py_builtin(
    builtin: str, keywords: Sequence[py.keyword], *, provenance: vcl.Provenance
) -> py.expr:
    """Make a builtin function call."""
    return py.Call(
        func=py.Attribute(
            value=py.Name(
                id="__vehicle__",
                ctx=py.Load(),
                **asdict(provenance),
            ),
            attr=builtin,
            ctx=py.Load(),
            **asdict(provenance),
        ),
        args=[],
        keywords=list(keywords),
        **asdict(provenance),
    )


################################################################################
### Compilation to Python
################################################################################


def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DIRECT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    translation: Optional[PythonTranslation] = None,
) -> Dict[str, Any]:
    # Ensure that specification_path is a Path
    if isinstance(specification_path, str):
        specification_path = Path(specification_path)

    # Load the specification file
    if session is not None:
        program = session.load(specification_path, target=target)
    else:
        program = global_session.load(specification_path, target=target)

    # Ensure that translation has its default value:
    if translation is None:
        translation = PythonTranslation(builtins=PythonBuiltins())

    # Translate the specification to a Python module:
    specification_filename = specification_path.name
    return translation.compile(program, specification_filename, context)
