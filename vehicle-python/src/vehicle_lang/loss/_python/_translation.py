import ast as py
from abc import ABCMeta
from dataclasses import asdict, dataclass, field
from fractions import Fraction
from functools import reduce
from pathlib import Path
from types import CodeType
from typing import Any, Iterator, Mapping, Sequence

import black

from vehicle_lang._temporary_files import VEHICLE_PATH

from ..._ast import _nodes as vcl
from .._abc import ABCSampler, AnyBuiltins, Index, Tensor


# Helper to raise a TypeError while compiling
def invalid_type(py_ast: py.Module | py.Expression, error: TypeError) -> TypeError:
    py_ast_str: str
    try:
        py_ast_str = py.unparse(py_ast)
    except Exception:
        py_ast_str = py.dump(py_ast)
    raise TypeError(f"{error}\n{py_ast_str}")


################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


class EraseType(Exception):
    pass


_IGNORED_RETURN_KEYS = {
    "__vehicle__",
    "__vehicle_user_samplers__",
    "__builtins__",
    "__annotations__",
}


@dataclass(frozen=True)
class PythonTranslation(metaclass=ABCMeta):
    builtins: AnyBuiltins
    module_header: Sequence[py.stmt] = field(default_factory=tuple)
    module_footer: Sequence[py.stmt] = field(default_factory=tuple)

    def compile(
        self, py_ast: py.Module | py.Expression, path: str | Path, mode: str
    ) -> CodeType:
        try:
            py_bytecode = compile(py_ast, filename=str(path), mode=mode)
        except TypeError as e:
            invalid_type(py_ast, e)
        return py_bytecode

    def compile_program(
        self,
        program: vcl.Program,
        path: str | Path,
        declaration_context: dict[str, Any],
        samplers: dict[str, Any],
    ) -> dict[str, Any]:
        py_ast = self.translate_program(program)
        try:
            declaration_context["__vehicle__"] = self.builtins
            declaration_context["__vehicle_user_samplers__"] = samplers
            before_exec = dict(declaration_context)

            # Write out the source code for debugging purposes (might make this optional in future if it harms performance)
            source_str = py.unparse(py_ast)
            formatted_source_str = black.format_str(source_str, mode=black.Mode())
            python_code_path = (
                VEHICLE_PATH / "generated_python" / (Path(path).stem + ".py")
            )
            python_code_path.parent.mkdir(exist_ok=True)
            python_code_path.write_text(formatted_source_str)

            py_bytecode = self.compile(
                formatted_source_str, path=str(python_code_path), mode="exec"
            )

            exec(py_bytecode, declaration_context)
        except TypeError as e:
            invalid_type(py_ast, e)
        return {
            key: value
            for key, value in declaration_context.items()
            if key not in _IGNORED_RETURN_KEYS
            and (key not in before_exec or before_exec[key] is not value)
        }

    def compile_expression(
        self,
        expression: vcl.Expression,
        path: str | Path,
        declaration_context: dict[str, Any],
    ) -> Any:
        expr = self.translate_expression(expression)
        py_ast = py.Expression(body=expr)

        declaration_context["__vehicle__"] = self.builtins

        py_bytecode = self.compile(py_ast, path, mode="eval")

        try:
            result = eval(py_bytecode, declaration_context)
            return result
        except TypeError as e:
            invalid_type(py_ast, e)

    def translate_program(self, program: vcl.Program) -> py.Module:
        match program:
            case vcl.Main():
                return self.translate_Main(program)
            case _:
                raise NotImplementedError(type(program).__name__)

    def translate_Main(self, program: vcl.Main) -> py.Module:
        return py.Module(
            body=[
                # NOTE: 'fractions' is imported for 'Fraction'
                #       which is used to translate vcl.Rat
                py.Import(
                    names=[
                        py.alias(name="fractions", asname=None, lineno=0, col_offset=0)
                    ],
                    lineno=0,
                    col_offset=0,
                ),
                *self.module_header,
                *self.translate_declarations(iter(program.declarations)),
                *self.module_footer,
            ],
            type_ignores=[],
        )

    def translate_declarations(
        self, declarations: Iterator[vcl.Declaration]
    ) -> Iterator[py.stmt]:
        for declaration in declarations:
            yield self.translate_declaration(declaration)

    def translate_declaration(self, declaration: vcl.Declaration) -> py.stmt:
        match declaration:
            case vcl.DefFunction():
                return self.translate_DefFunction(declaration)
            case _:
                raise NotImplementedError(type(declaration).__name__)

    def translate_binder(self, binder: vcl.Binder) -> py.arg:
        return py.arg(
            arg=binder.name,
            annotation=None,
            **asdict(binder.provenance),
        )

    def translate_expression(self, expression: vcl.Expression) -> py.expr:
        match expression:
            case vcl.Var():
                return self.translate_Var(expression)
            case vcl.Lam():
                return self.translate_Lam(expression)
            case vcl.Pi():
                return self.translate_Pi(expression)
            case vcl.Let():
                return self.translate_Let(expression)
            case vcl.Record():
                return self.translate_Record(expression)
            case vcl.RecordAcc():
                return self.translate_RecordAcc(expression)
            case vcl.BoolTensor():
                return self.translate_BoolTensor(expression)
            case vcl.BoolNot():
                return self.translate_BoolNot(expression)
            case vcl.BoolAnd():
                return self.translate_BoolAnd(expression)
            case vcl.BoolOr():
                return self.translate_BoolOr(expression)
            case vcl.BoolImplies():
                return self.translate_BoolImplies(expression)
            case vcl.BoolCompareIndex():
                return self.translate_BoolCompareIndex(expression)
            case vcl.BoolCompareNat():
                return self.translate_BoolCompareNat(expression)
            case vcl.BoolCompareRatTensor():
                return self.translate_BoolCompareRatTensor(expression)
            case vcl.BoolReduceAnd():
                return self.translate_BoolReduceAnd(expression)
            case vcl.BoolReduceOr():
                return self.translate_BoolReduceOr(expression)
            case vcl.BoolIf():
                return self.translate_BoolIf(expression)
            case vcl.RatTensor():
                return self.translate_RatTensor(expression)
            case vcl.AddRatTensor():
                return self.translate_AddRatTensor(expression)
            case vcl.SubRatTensor():
                return self.translate_SubRatTensor(expression)
            case vcl.MulRatTensor():
                return self.translate_MulRatTensor(expression)
            case vcl.DivRatTensor():
                return self.translate_DivRatTensor(expression)
            case vcl.MinRatTensor():
                return self.translate_MinRatTensor(expression)
            case vcl.MaxRatTensor():
                return self.translate_MaxRatTensor(expression)
            case vcl.PowRatTensor():
                return self.translate_PowRatTensor(expression)
            case vcl.LogRatTensor():
                return self.translate_LogRatTensor(expression)
            case vcl.ExpRatTensor():
                return self.translate_ExpRatTensor(expression)
            case vcl.NegRatTensor():
                return self.translate_NegRatTensor(expression)
            case vcl.ReduceAddRatTensor():
                return self.translate_ReduceAddRatTensor(expression)
            case vcl.ReduceMulRatTensor():
                return self.translate_ReduceMulRatTensor(expression)
            case vcl.ReduceMinRatTensor():
                return self.translate_ReduceMinRatTensor(expression)
            case vcl.ReduceMaxRatTensor():
                return self.translate_ReduceMaxRatTensor(expression)
            case vcl.SearchRatTensor():
                return self.translate_SearchRatTensor(expression)
            case vcl.WhereTensor():
                return self.translate_WhereTensor(expression)
            case vcl.Dimension():
                return self.translate_Dimension(expression)
            case vcl.DimensionCons():
                return self.translate_DimensionCons(expression)
            case vcl.DimensionIndex():
                return self.translate_DimensionIndex(expression)
            case vcl.DimensionNil():
                return self.translate_DimensionNil(expression)
            case vcl.ConstTensor():
                return self.translate_ConstTensor(expression)
            case vcl.Transpose():
                return self.translate_Transpose(expression)
            case vcl.StackTensor():
                return self.translate_StackTensor(expression)
            case vcl.AtTensor():
                return self.translate_AtTensor(expression)
            case vcl.ForeachTensor():
                return self.translate_ForeachTensor(expression)
            case vcl.VectorLiteral():
                return self.translate_VectorLiteral(expression)
            case vcl.AtVector():
                return self.translate_AtVector(expression)
            case vcl.ForeachVector():
                return self.translate_ForeachVector(expression)
            case _:
                raise NotImplementedError(type(expression).__name__)

    def translate_DefFunction(self, declaration: vcl.DefFunction) -> py.stmt:
        body = declaration.body
        binders = []
        while isinstance(body, vcl.Lam):
            binders.append(self.translate_binder(body.binder))
            body = body.body

        if binders:
            return py.FunctionDef(
                name=declaration.name,
                args=py_binder(*binders),
                body=[
                    py.Return(
                        value=self.translate_expression(body),
                        **asdict(vcl.MISSING),
                    )
                ],
                decorator_list=[],
                **asdict(vcl.MISSING),
            )
        else:
            return py.Assign(
                targets=[
                    py.Name(
                        id=declaration.name,
                        ctx=py.Store(),
                        **asdict(vcl.MISSING),
                    )
                ],
                value=self.translate_expression(declaration.body),
                **asdict(vcl.MISSING),
            )

    def translate_Var(self, expression: vcl.Var) -> py.expr:
        return py_app(
            py_name(expression.name),
            *map(self.translate_expression, expression.arguments),
        )

    def translate_Lam(self, expression: vcl.Lam) -> py.expr:
        return py.Lambda(
            args=py_binder(self.translate_binder(expression.binder)),
            body=self.translate_expression(expression.body),
            **asdict(vcl.MISSING),
        )

    def translate_Pi(self, expression: vcl.Pi) -> py.expr:
        raise EraseType()

    def translate_Let(self, expression: vcl.Let) -> py.expr:
        return py_app(
            py.Lambda(
                args=py_binder(self.translate_binder(expression.binder)),
                body=self.translate_expression(expression.body),
                **asdict(vcl.MISSING),
            ),
            self.translate_expression(expression.bound),
        )

    def translate_Record(self, expression: vcl.Record) -> py.expr:
        """Translate Record to a dictionary literal."""
        fields = expression.fields

        keys: list[py.expr | None] = []
        values: list[py.expr] = []
        for field_name, field_value in fields:
            keys.append(py.Constant(value=field_name, **asdict(vcl.MISSING)))
            values.append(self.translate_expression(field_value))

        result = py.Dict(keys=keys, values=values, **asdict(vcl.MISSING))
        return result

    def translate_RecordAcc(self, expression: vcl.RecordAcc) -> py.expr:
        """Translate record accessor to '<expr>[field](<args>)."""
        return py_app_sequential(
            function=py.Subscript(
                value=self.translate_expression(expression.record),
                slice=py.Constant(value=expression.field, **asdict(vcl.MISSING)),
                ctx=py.Load(),
                **asdict(vcl.MISSING),
            ),
            arguments=[self.translate_expression(arg) for arg in expression.arguments],
        )

    def translate_BoolTensor(self, expression: vcl.BoolTensor) -> py.expr:
        return py_tensor(expression.contents)

    def translate_BoolNot(self, expression: vcl.BoolNot) -> py.expr:
        return py_app(py_builtin("BoolNot"), self.translate_expression(expression.x))

    def translate_BoolAnd(self, expression: vcl.BoolAnd) -> py.expr:
        return py_app(
            py_builtin("BoolAnd"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_BoolOr(self, expression: vcl.BoolOr) -> py.expr:
        return py_app(
            py_builtin("BoolOr"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_BoolImplies(self, expression: vcl.BoolImplies) -> py.expr:
        return py_app(
            py_builtin("BoolImplies"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_BoolCompareIndex(self, expression: vcl.BoolCompareIndex) -> py.expr:
        return py_app(
            py_builtin("BoolCompareIndex"),
            py.Constant(value=expression.op, **asdict(vcl.MISSING)),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_BoolCompareNat(self, expression: vcl.BoolCompareNat) -> py.expr:
        return py_app(
            py_builtin("BoolCompareNat"),
            py.Constant(value=expression.op, **asdict(vcl.MISSING)),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_BoolCompareRatTensor(
        self, expression: vcl.BoolCompareRatTensor
    ) -> py.expr:
        return py_app(
            py_builtin("BoolCompareRatTensor"),
            py.Constant(value=expression.op, **asdict(vcl.MISSING)),
            self.translate_expression(expression.p_dims),
            self.translate_expression(expression.r_dims),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_BoolReduceAnd(self, expression: vcl.BoolReduceAnd) -> py.expr:
        return py_app(
            py_builtin("BoolReduceAnd"), self.translate_expression(expression.x)
        )

    def translate_BoolReduceOr(self, expression: vcl.BoolReduceOr) -> py.expr:
        return py_app(
            py_builtin("BoolReduceOr"), self.translate_expression(expression.x)
        )

    def translate_BoolIf(self, expression: vcl.BoolIf) -> py.expr:
        return py_app(
            py_builtin("BoolIf"),
            self.translate_expression(expression.c),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_RatTensor(self, expression: vcl.RatTensor) -> py.expr:
        """Translate RatTensor to tensor creation."""
        return py_tensor(expression.contents)

    def translate_AddRatTensor(self, expression: vcl.AddRatTensor) -> py.expr:
        """Translate AddRatTensor to builtin call."""
        return py_app(
            py_builtin("AddRatTensor"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_SubRatTensor(self, expression: vcl.SubRatTensor) -> py.expr:
        """Translate SubRatTensor to builtin call."""
        return py_app(
            py_builtin("SubRatTensor"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_MulRatTensor(self, expression: vcl.MulRatTensor) -> py.expr:
        """Translate MulRatTensor to builtin call."""
        return py_app(
            py_builtin("MulRatTensor"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_DivRatTensor(self, expression: vcl.DivRatTensor) -> py.expr:
        """Translate DivRatTensor to builtin call."""
        return py_app(
            py_builtin("DivRatTensor"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_MinRatTensor(self, expression: vcl.MinRatTensor) -> py.expr:
        """Translate MinRatTensor to builtin call."""
        return py_app(
            py_builtin("MinRatTensor"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_MaxRatTensor(self, expression: vcl.MaxRatTensor) -> py.expr:
        """Translate MaxRatTensor to builtin call."""
        return py_app(
            py_builtin("MaxRatTensor"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_PowRatTensor(self, expression: vcl.PowRatTensor) -> py.expr:
        """Translate PowRatTensor to builtin call."""
        return py_app(
            py_builtin("PowRatTensor"),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
        )

    def translate_LogRatTensor(self, expression: vcl.LogRatTensor) -> py.expr:
        """Translate LogRatTensor to builtin call."""
        return py_app(
            py_builtin("LogRatTensor"), self.translate_expression(expression.x)
        )

    def translate_ExpRatTensor(self, expression: vcl.ExpRatTensor) -> py.expr:
        """Translate ExpRatTensor to builtin call."""
        return py_app(
            py_builtin("ExpRatTensor"), self.translate_expression(expression.x)
        )

    def translate_NegRatTensor(self, expression: vcl.NegRatTensor) -> py.expr:
        """Translate NegRatTensor to builtin call."""
        return py_app(
            py_builtin("NegRatTensor"), self.translate_expression(expression.x)
        )

    def translate_ReduceAddRatTensor(
        self, expression: vcl.ReduceAddRatTensor
    ) -> py.expr:
        """Translate ReduceAddRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceAddRatTensor"), self.translate_expression(expression.x)
        )

    def translate_ReduceMulRatTensor(
        self, expression: vcl.ReduceMulRatTensor
    ) -> py.expr:
        """Translate ReduceMulRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceMulRatTensor"), self.translate_expression(expression.x)
        )

    def translate_ReduceMinRatTensor(
        self, expression: vcl.ReduceMinRatTensor
    ) -> py.expr:
        """Translate ReduceMinRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceMinRatTensor"), self.translate_expression(expression.x)
        )

    def translate_ReduceMaxRatTensor(
        self, expression: vcl.ReduceMaxRatTensor
    ) -> py.expr:
        """Translate ReduceMaxRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceMaxRatTensor"), self.translate_expression(expression.x)
        )

    def translate_SearchRatTensor(self, expression: vcl.SearchRatTensor) -> py.expr:
        """Translate SearchRatTensor to builtin call."""
        # Call sampler once to get samples
        sampler_call = py_app(
            py_subscript(
                py_qualified_name("__vehicle_user_samplers__"),
                py.Constant(value=expression.name, **asdict(vcl.MISSING)),
            ),
            self.translate_expression(expression.dims),
            self.translate_expression(expression.lower_bound),
            self.translate_expression(expression.upper_bound),
            self.translate_expression(expression.search_lambda),
        )

        return py_app(py_builtin("ReduceMaxRatTensor"), sampler_call)

    def translate_WhereTensor(self, expression: vcl.WhereTensor) -> py.expr:
        """Translate WhereTensor to builtin call."""
        return py_app(
            py_builtin("WhereTensor"),
            self.translate_expression(expression.input_tensor),
            self.translate_expression(expression.condition),
            self.translate_expression(expression.false_value),
        )

    def translate_Dimension(self, expression: vcl.Dimension) -> py.expr:
        """Translate Dimension to constant."""
        return py.Constant(value=expression.value, **asdict(vcl.MISSING))

    def translate_DimensionCons(self, expression: vcl.DimensionCons) -> py.expr:
        """Translate DimensionCons to builtin call."""
        return py_app(
            py_builtin("DimensionCons"),
            self.translate_expression(expression.e1),
            self.translate_expression(expression.e2),
        )

    def translate_DimensionIndex(self, expression: vcl.DimensionIndex) -> py.expr:
        """Translate DimensionIndex to constant."""
        return py.Constant(value=expression.i, **asdict(vcl.MISSING))

    def translate_DimensionNil(self, expression: vcl.DimensionNil) -> py.expr:
        """Translate DimensionNil to empty tuple."""
        return py_tuple([])

    def translate_ConstTensor(self, expression: vcl.ConstTensor) -> py.expr:
        """Translate ConstTensor to builtin call."""
        return py_app(
            py_builtin("ConstTensor"),
            self.translate_expression(expression.c),
            self.translate_expression(expression.ds),
        )

    def translate_StackTensor(self, expression: vcl.StackTensor) -> py.expr:
        """Translate StackTensor to builtin call."""
        return py_app(
            py_builtin("StackTensor"),
            py_tuple([self.translate_expression(x) for x in expression.xs]),
        )

    def translate_Transpose(self, expression: vcl.Transpose) -> py.expr:
        """Translate Transpose to builtin call."""
        return py_app(py_builtin("Transpose"), self.translate_expression(expression.xs))

    def translate_AtTensor(self, expression: vcl.AtTensor) -> py.expr:
        """Translate AtTensor to builtin call."""
        return py_app(
            py_builtin("AtTensor"),
            self.translate_expression(expression.xs),
            self.translate_expression(expression.i),
        )

    def translate_ForeachTensor(self, expression: vcl.ForeachTensor) -> py.expr:
        """Translate ForeachTensor to builtin call."""
        return py_app(
            py_builtin("ForeachTensor"),
            self.translate_expression(expression.size),
            self.translate_expression(expression.function),
        )

    def translate_VectorLiteral(self, expression: vcl.VectorLiteral) -> py.expr:
        """Translate VectorLiteral to builtin call."""
        return py_app(
            py_builtin("VectorLiteral"),
            py_tuple([self.translate_expression(x) for x in expression.elements]),
        )

    def translate_AtVector(self, expression: vcl.AtVector) -> py.expr:
        """Translate AtVector to builtin call."""
        return py_app(
            py_builtin("AtVector"),
            self.translate_expression(expression.xs),
            self.translate_expression(expression.i),
        )

    def translate_ForeachVector(self, expression: vcl.ForeachVector) -> py.expr:
        """Translate ForeachVector to builtin call."""
        return py_app(
            py_builtin("ForeachVector"),
            self.translate_expression(expression.size),
            self.translate_expression(expression.function),
        )


################################################################################
### Helper methods
################################################################################


def py_name(name: vcl.Name) -> py.Name:
    """Make a name."""
    return py.Name(
        id=name,
        ctx=py.Load(),
        **asdict(vcl.MISSING),
    )


def py_qualified_name(*parts: vcl.Name) -> py.expr:
    """Make a qualified name."""
    if not parts:
        raise ValueError("A qualified name should have at least one part.")

    def py_attribute(value: py.expr, attr: str) -> py.expr:
        return py.Attribute(
            value=value, attr=attr, ctx=py.Load(), **asdict(vcl.MISSING)
        )

    initial: py.expr = py_name(parts[0])
    return reduce(py_attribute, parts[1:], initial)


def py_binder(*args: py.arg) -> py.arguments:
    """Make a binder which only uses args."""
    return py.arguments(
        posonlyargs=[],
        args=list(args),
        vararg=None,
        kwonlyargs=[],
        kw_defaults=[],
        kwarg=None,
        defaults=[],
    )


def py_builtin(builtin: str) -> py.expr:
    """Make a builtin function call."""
    return py_qualified_name("__vehicle__", builtin)


def py_subscript(value: py.expr, slice: py.expr) -> py.expr:
    """Make a subscript expression."""
    return py.Subscript(value=value, slice=slice, ctx=py.Load(), **asdict(vcl.MISSING))


def py_app(function: py.expr, *arguments: py.expr) -> py.expr:
    """Make a function call: function(arguments[0],...,arguments[n])"""
    if not arguments:
        return function

    return py.Call(
        func=function,
        args=list(arguments),
        keywords=[],
        **asdict(vcl.MISSING),
    )


def py_app_sequential(function: py.expr, arguments: Sequence[py.expr]) -> py.expr:
    """Make a series of function calls: function(arguments[0])...(arguments[n])."""
    if not arguments:
        return function

    return py_app_sequential(
        function=py.Call(
            func=function,
            args=[arguments[0]],
            keywords=[],
            **asdict(vcl.MISSING),
        ),
        arguments=arguments[1:],
    )


def py_fraction(value: Fraction) -> py.expr:
    return py_app(
        py_qualified_name("fractions", "Fraction"),
        py.Constant(
            value=value.numerator,
            **asdict(vcl.MISSING),
        ),
        py.Constant(
            value=value.denominator,
            **asdict(vcl.MISSING),
        ),
    )


def py_extended_fraction(value: vcl.ExtendedFraction) -> py.expr:
    match value:
        case vcl.Finite(value=inner):
            return py_fraction(inner)
        case vcl.PosInfinity():
            return py.Constant(value=float("inf"), **asdict(vcl.MISSING))
        case vcl.NegInfinity():
            return py.Constant(value=float("-inf"), **asdict(vcl.MISSING))
        case _:
            raise ValueError(f"Unknown extended rational type: {type(value)}")


def py_scalar(value: vcl.DType) -> py.expr:
    """Make a scalar."""
    match value:
        case vcl.ExtendedFraction():
            return py_extended_fraction(value)
        case _:
            return py.Constant(
                value=value,
                **asdict(vcl.MISSING),
            )


def py_tuple(elements: list[py.expr]) -> py.expr:
    """Make a tuple."""
    return py.Tuple(
        elts=list(elements),
        ctx=py.Load(),
        **asdict(vcl.MISSING),
    )


def py_tensor(tensor: vcl.Tensor[vcl.DType]) -> py.expr:
    """Make a tensor by calling appropriate builtin."""
    match tensor:
        case vcl.DenseTensor():
            # DenseTensor: call __vehicle__.DenseTensor(values, shape)
            return py_app(
                py_builtin("DenseTensor"),
                py_tuple([py_scalar(val) for val in tensor.values]),
                py_tuple(
                    [
                        py.Constant(value=dim, **asdict(vcl.MISSING))
                        for dim in tensor.shape
                    ]
                ),
            )
        case vcl.ConstantTensor():
            # ConstantTensor: call __vehicle__.ConstTensor(value, shape)
            return py_app(
                py_builtin("ConstTensor"),
                py_scalar(tensor.value),
                py_tuple(
                    [
                        py.Constant(value=dim, **asdict(vcl.MISSING))
                        for dim in tensor.shape
                    ]
                ),
            )
        case _:
            raise ValueError(f"Unknown tensor type: {type(tensor)}")
