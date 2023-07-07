import ast as py
import operator
from dataclasses import asdict, dataclass, field
from fractions import Fraction
from functools import partial, reduce
from numbers import Integral, Rational
from pathlib import Path
from typing import Any, Dict, List, NoReturn, Optional, Sequence, Type, Union

from typing_extensions import TypeVar, overload, override

from .. import ast as vcl
from .. import session as global_session
from .._target import Target
from ..session import Session
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
from .abc import ABCTranslation, Builtins, Sampler

_T = TypeVar("_T")

################################################################################
### Implementation of Vehicle builtins in Python
################################################################################


@dataclass(frozen=True)
class PythonBuiltins(
    Builtins[
        bool,
        int,
        int,
        int,
        float,
        Any,
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
        return lambda x: lambda y: x and y

    @override
    def AtVector(self) -> Function2[Sequence[_T], int, _T]:
        return curry(operator.getitem)

    @override
    def Bool(self, value: bool) -> bool:
        return value.__bool__()

    @override
    def BoolType(self) -> Type[bool]:
        return bool

    @override
    def DivRat(self) -> Operator2[float]:
        return curry(operator.truediv)

    @override
    def Eq(self) -> Relation2[Any, bool]:
        return curry(operator.eq)

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
    def Index(self, value: Integral) -> int:
        return value.__int__()

    @override
    def IndexType(self) -> Type[int]:
        return int

    @override
    def Indices(self) -> Function1[int, Sequence[int]]:
        return partial(range, 0)

    @override
    def Int(self, value: Integral) -> int:
        return value.__int__()

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
    def Nat(self, value: Integral) -> int:
        return value.__int__()

    @override
    def NatType(self) -> Type[int]:
        return int

    @override
    def Ne(self) -> Relation2[Any, bool]:
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
        return lambda x: lambda y: x or y

    @override
    def PowRat(self) -> Operator2[float]:
        return curry(operator.pow)

    @override
    def Rat(self, value: Rational) -> float:
        return value.__float__()

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
    builtins: Builtins[Any, Any, Any, Any, Any, Any]
    module_header: Sequence[py.stmt] = field(default_factory=tuple)
    module_footer: Sequence[py.stmt] = field(default_factory=tuple)

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
            raise TypeError(f"{e}\n{py_ast_str}")

    def translate_Main(self, program: vcl.Main) -> py.Module:
        return py.Module(
            body=[
                py.ImportFrom(
                    module="fractions",
                    names=[
                        py.alias(name="Fraction", asname=None, **asdict(vcl.MISSING))
                    ],
                    level=0,
                    **asdict(vcl.MISSING),
                ),
                *self.module_header,
                *map(self.translate_declaration, program.declarations),
                *self.module_footer,
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
                        func=py_name("vars", declaration.provenance),
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
                builtin="Vector",
                keywords=[
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
        # NOTE: Quantifiers are handled separately:
        #       Vehicle represents Quantifiers as builtins, but in order
        #       to interpret them using sampling, we must have access to
        #       the name of the quantified variable.
        if isinstance(expression.func, vcl.BuiltinOp) and isinstance(
            expression.func.builtin, (vcl.Forall, vcl.Exists)
        ):
            quantifier = expression.func.builtin
            assert (
                len(expression.args) == 1
            ), f"Found {quantifier.__class__.__name__} with {len(expression.args)} arguments."
            assert isinstance(
                expression.args[0], vcl.Lam
            ), f"Found {quantifier.__class__.__name__} applied to {expression.args[0].__class__.__name__}"
            lam = expression.args[0]
            return py_app(
                func=py_builtin(
                    builtin=quantifier.__class__.__name__,
                    keywords=[
                        py.keyword(
                            arg="name",
                            value=py.Str(
                                s=lam.binder.name,
                                **asdict(lam.provenance),
                            ),
                            **asdict(lam.provenance),
                        ),
                        py.keyword(
                            arg="context",
                            value=py.Call(
                                func=py_name("locals", lam.provenance),
                                args=[],
                                keywords=[],
                                **asdict(lam.provenance),
                            ),
                            **asdict(lam.provenance),
                        ),
                    ],
                    provenance=expression.func.provenance,
                ),
                arg=self.translate_expression(lam),
                provenance=expression.provenance,
            )

        return reduce(
            partial(py_app, provenance=expression.provenance),
            [self.translate_expression(arg) for arg in expression.args],
            self.translate_expression(expression.func),
        )

    def translate_BoundVar(self, expression: vcl.BoundVar) -> py.expr:
        return py_name(expression.name, expression.provenance)

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
                    arg="value",
                    value=py.Call(
                        func=py_name("Fraction", expression.provenance),
                        args=[],
                        keywords=[
                            py.keyword(
                                arg="numerator",
                                value=py.Num(
                                    n=expression.builtin.numerator,
                                    **asdict(expression.provenance),
                                ),
                                **asdict(expression.provenance),
                            ),
                            py.keyword(
                                arg="denominator",
                                value=py.Num(
                                    n=expression.builtin.denominator,
                                    **asdict(expression.provenance),
                                ),
                                **asdict(expression.provenance),
                            ),
                        ],
                        **asdict(expression.provenance),
                    ),
                    **asdict(expression.provenance),
                )
            )
        elif isinstance(expression.builtin, vcl.Sample):
            keywords.append(
                py.keyword(
                    arg="name",
                    value=py.Str(
                        s=expression.builtin.name, **asdict(expression.provenance)
                    ),
                    **asdict(expression.provenance),
                )
            )
            keywords.append(
                py.keyword(
                    arg="context",
                    value=py.Dict(
                        keys=[
                            py.Str(s=local, **asdict(expression.provenance))
                            for local in expression.builtin.locals
                        ],
                        values=[
                            py_name(local, expression.provenance)
                            for local in expression.builtin.locals
                        ],
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
        return py_name(expression.name, expression.provenance)

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


def py_name(name: vcl.Name, provenance: vcl.Provenance) -> py.Name:
    """Make a name."""
    return py.Name(
        id=name,
        ctx=py.Load(),
        **asdict(provenance),
    )


def py_sampler(
    name: vcl.Name, locals: Sequence[vcl.Name], *, provenance: vcl.Provenance
) -> py.expr:
    """Make a call to a sampler."""
    return py.Call(
        func=py_name(f"sampler_for_{name}", provenance),
        args=[
            py.Dict(
                keys=[py.Str(s=local, **asdict(provenance)) for local in locals],
                values=[py_name(local, provenance) for local in locals],
                **asdict(provenance),
            )
        ],
        keywords=[],
        **asdict(provenance),
    )


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


def py_app(func: py.expr, arg: py.expr, *, provenance: vcl.Provenance) -> py.expr:
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
            value=py_name("__vehicle__", provenance),
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


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: Dict[str, Sampler],
    builtins: None = None,
    translation: None = None,
) -> Dict[str, Any]:
    ...


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: None = None,
    builtins: Builtins[Any, Any, Any, Any, Any, Any],
    translation: None = None,
) -> Dict[str, Any]:
    ...


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: None = None,
    builtins: None = None,
    translation: PythonTranslation,
) -> Dict[str, Any]:
    ...


@overload
def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: None = None,
    builtins: None = None,
    translation: None = None,
) -> Dict[str, Any]:
    ...


def to_python(
    specification_path: Union[str, Path],
    *,
    target: Target = Target.DEFAULT,
    context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    samplers: Optional[Dict[str, Sampler]] = None,
    builtins: Optional[Builtins[Any, Any, Any, Any, Any, Any]] = None,
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

    # The user can provide one of samplers, builtins, or translation:
    if samplers is not None:
        assert (
            builtins is None and translation is None
        ), "Only one of 'samplers', 'builtins', or 'translation' may be specified."
        translation = PythonTranslation(builtins=PythonBuiltins(samplers=samplers))
    elif builtins is not None:
        assert (
            samplers is None and translation is None
        ), "Only one of 'samplers', 'builtins', or 'translation' may be specified."
        translation = PythonTranslation(builtins=builtins)
    elif translation is not None:
        assert (
            samplers is None and builtins is None
        ), "Only one of 'samplers', 'builtins', or 'translation' may be specified."
    else:
        translation = PythonTranslation(builtins=PythonBuiltins())

    # Translate the specification to a Python module:
    specification_filename = specification_path.name
    return translation.compile(program, specification_filename, context)
