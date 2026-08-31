import ast as py
from dataclasses import asdict, dataclass, field
from fractions import Fraction
from functools import reduce
from pathlib import Path
from typing import Any, Iterator, Mapping, Sequence

from ..._ast import _nodes as vcl
from .._abc import ABCSampler, ABCTranslation, AnyBuiltins, Index, Tensor


# Helper to convert Vehicle provenance to Python AST kwargs
def py_provenance(provenance: vcl.Provenance) -> dict[str, Any]:
    """Convert Vehicle provenance to Python AST keyword arguments"""
    return {
        "lineno": provenance.lineno or 0,
        "col_offset": provenance.col_offset or 0,
    }


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
class PythonTranslation(ABCTranslation[py.Module, py.stmt, py.expr]):
    builtins: AnyBuiltins
    module_header: Sequence[py.stmt] = field(default_factory=tuple)
    module_footer: Sequence[py.stmt] = field(default_factory=tuple)
    ignored_types: list[str] = field(init=False, default_factory=list)

    def compile(
        self,
        program: vcl.Program,
        path: str | Path,
        declaration_context: dict[str, Any],
        samplers: Mapping[str, ABCSampler[Index, Tensor]],
    ) -> dict[str, Any]:
        py_ast = self.translate_program(program)
        try:
            declaration_context["__vehicle__"] = self.builtins
            declaration_context["__vehicle_user_samplers__"] = samplers
            before_exec = dict(declaration_context)
            py_bytecode = compile(py_ast, filename=str(path), mode="exec")
            exec(py_bytecode, declaration_context)
            return {
                key: value
                for key, value in declaration_context.items()
                if key not in _IGNORED_RETURN_KEYS
                and (key not in before_exec or before_exec[key] is not value)
            }
        except TypeError as e:
            py_ast_str: str
            try:
                py_ast_str = py.unparse(py_ast)
            except Exception:
                py_ast_str = py.dump(py_ast)
            raise TypeError(f"{e}\n{py_ast_str}")

    def translate_Main(self, program: vcl.Main) -> py.Module:
        return py.Module(
            body=[
                # NOTE: 'vehicle_lang._ast._nodes' is imported for 'Tensor'
                #       which is used to translate vcl.Tensor
                py.Import(
                    names=[
                        py.alias(
                            name="vehicle_lang._ast._nodes",
                            asname=None,
                            lineno=0,
                            col_offset=0,
                        )
                    ],
                    lineno=0,
                    col_offset=0,
                ),
                # NOTE: 'fractions' is imported for 'Fraction'
                #       which is used to translate vcl.Rat
                py.Import(
                    names=[
                        py.alias(name="fractions", asname=None, lineno=0, col_offset=0)
                    ],
                    lineno=0,
                    col_offset=0,
                ),
                # NOTE: 'functools' is imported for 'partial'
                #       which is used to translate vcl.PartialApp
                py.Import(
                    names=[
                        py.alias(name="functools", asname=None, lineno=0, col_offset=0)
                    ],
                    **py_provenance(vcl.MISSING),
                ),
                *self.module_header,
                *self.translate_declarations(iter(program.declarations)),
                *self.module_footer,
            ],
            type_ignores=[],
        )

    def translate_binder(self, binder: vcl.Binder) -> py.arg:
        return py.arg(
            arg=binder.name or "_",  # TODO: check why name can be None
            annotation=None,
            **asdict(binder.provenance),
        )

    def translate_declarations(
        self, declarations: Iterator[vcl.Declaration]
    ) -> Iterator[py.stmt]:
        for declaration in declarations:
            try:
                yield self.translate_declaration(declaration)
            except EraseType:
                name = declaration.get_name()
                self.ignored_types.append(name)

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
                        **asdict(declaration.provenance),
                    )
                ],
                decorator_list=[],
                **asdict(declaration.provenance),
            )
        else:
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
        return py_app(
            self.translate_expression(expression.function),
            *map(self.translate_expression, expression.arguments),
            provenance=expression.provenance,
        )

    def translate_Var(self, expression: vcl.Var) -> py.expr:
        return py_app_sequential(
            function=py_name(expression.name, provenance=vcl.MISSING),
            arguments=[self.translate_expression(arg) for arg in expression.arguments],
            provenance=vcl.MISSING,
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
            provenance=vcl.MISSING,
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
            provenance=vcl.MISSING,
        )

    def translate_BoolTensor(self, expression: vcl.BoolTensor) -> py.expr:
        return py_tensor(expression.contents, provenance=vcl.MISSING)

    def translate_BoolNot(self, expression: vcl.BoolNot) -> py.expr:
        return py_app(
            py_builtin("BoolNot", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_BoolAnd(self, expression: vcl.BoolAnd) -> py.expr:
        return py_app(
            py_builtin("BoolAnd", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_BoolOr(self, expression: vcl.BoolOr) -> py.expr:
        return py_app(
            py_builtin("BoolOr", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_BoolImplies(self, expression: vcl.BoolImplies) -> py.expr:
        return py_app(
            py_builtin("BoolImplies", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_BoolCompareIndex(self, expression: vcl.BoolCompareIndex) -> py.expr:
        return py_app(
            py_builtin("BoolCompareIndex", provenance=vcl.MISSING),
            py.Constant(value=expression.op, **asdict(vcl.MISSING)),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_BoolCompareNat(self, expression: vcl.BoolCompareNat) -> py.expr:
        return py_app(
            py_builtin("BoolCompareNat", provenance=vcl.MISSING),
            py.Constant(value=expression.op, **asdict(vcl.MISSING)),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_BoolCompareRatTensor(
        self, expression: vcl.BoolCompareRatTensor
    ) -> py.expr:
        return py_app(
            py_builtin("BoolCompareRatTensor", provenance=vcl.MISSING),
            py.Constant(value=expression.op, **asdict(vcl.MISSING)),
            self.translate_expression(expression.p_dims),
            self.translate_expression(expression.r_dims),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_BoolReduceAnd(self, expression: vcl.BoolReduceAnd) -> py.expr:
        return py_app(
            py_builtin("BoolReduceAnd", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_BoolReduceOr(self, expression: vcl.BoolReduceOr) -> py.expr:
        return py_app(
            py_builtin("BoolReduceOr", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_BoolIf(self, expression: vcl.BoolIf) -> py.expr:
        return py_app(
            py_builtin("BoolIf", provenance=vcl.MISSING),
            self.translate_expression(expression.c),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_RatTensor(self, expression: vcl.RatTensor) -> py.expr:
        """Translate RatTensor to tensor creation."""
        return py_tensor(expression.contents, provenance=vcl.MISSING)

    def translate_AddRatTensor(self, expression: vcl.AddRatTensor) -> py.expr:
        """Translate AddRatTensor to builtin call."""
        return py_app(
            py_builtin("AddRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_SubRatTensor(self, expression: vcl.SubRatTensor) -> py.expr:
        """Translate SubRatTensor to builtin call."""
        return py_app(
            py_builtin("SubRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_MulRatTensor(self, expression: vcl.MulRatTensor) -> py.expr:
        """Translate MulRatTensor to builtin call."""
        return py_app(
            py_builtin("MulRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_DivRatTensor(self, expression: vcl.DivRatTensor) -> py.expr:
        """Translate DivRatTensor to builtin call."""
        return py_app(
            py_builtin("DivRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_MinRatTensor(self, expression: vcl.MinRatTensor) -> py.expr:
        """Translate MinRatTensor to builtin call."""
        return py_app(
            py_builtin("MinRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_MaxRatTensor(self, expression: vcl.MaxRatTensor) -> py.expr:
        """Translate MaxRatTensor to builtin call."""
        return py_app(
            py_builtin("MaxRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_PowRatTensor(self, expression: vcl.PowRatTensor) -> py.expr:
        """Translate PowRatTensor to builtin call."""
        return py_app(
            py_builtin("PowRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            self.translate_expression(expression.y),
            provenance=vcl.MISSING,
        )

    def translate_LogRatTensor(self, expression: vcl.LogRatTensor) -> py.expr:
        """Translate LogRatTensor to builtin call."""
        return py_app(
            py_builtin("LogRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_ExpRatTensor(self, expression: vcl.ExpRatTensor) -> py.expr:
        """Translate ExpRatTensor to builtin call."""
        return py_app(
            py_builtin("ExpRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_NegRatTensor(self, expression: vcl.NegRatTensor) -> py.expr:
        """Translate NegRatTensor to builtin call."""
        return py_app(
            py_builtin("NegRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_ReduceAddRatTensor(
        self, expression: vcl.ReduceAddRatTensor
    ) -> py.expr:
        """Translate ReduceAddRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceAddRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_ReduceMulRatTensor(
        self, expression: vcl.ReduceMulRatTensor
    ) -> py.expr:
        """Translate ReduceMulRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceMulRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_ReduceMinRatTensor(
        self, expression: vcl.ReduceMinRatTensor
    ) -> py.expr:
        """Translate ReduceMinRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceMinRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_ReduceMaxRatTensor(
        self, expression: vcl.ReduceMaxRatTensor
    ) -> py.expr:
        """Translate ReduceMaxRatTensor to builtin call."""
        return py_app(
            py_builtin("ReduceMaxRatTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.x),
            provenance=vcl.MISSING,
        )

    def translate_SearchRatTensor(self, expression: vcl.SearchRatTensor) -> py.expr:
        """Translate SearchRatTensor to builtin call."""
        # Call sampler once to get samples
        sampler_call = py_app(
            py_subscript(
                py_qualified_name("__vehicle_user_samplers__", provenance=vcl.MISSING),
                py.Constant(value=expression.name, **asdict(vcl.MISSING)),
                provenance=vcl.MISSING,
            ),
            self.translate_expression(expression.dims),
            self.translate_expression(expression.lower_bound),
            self.translate_expression(expression.upper_bound),
            self.translate_expression(expression.search_lambda),
            provenance=vcl.MISSING,
        )

        return py_app(
            py_builtin("ReduceMaxRatTensor", provenance=vcl.MISSING),
            sampler_call,
            provenance=vcl.MISSING,
        )

    def translate_WhereTensor(self, expression: vcl.WhereTensor) -> py.expr:
        """Translate WhereTensor to builtin call."""
        return py_app(
            py_builtin("WhereTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.input_tensor),
            self.translate_expression(expression.condition),
            self.translate_expression(expression.false_value),
            provenance=vcl.MISSING,
        )

    def translate_Dimension(self, expression: vcl.Dimension) -> py.expr:
        """Translate Dimension to constant."""
        return py.Constant(value=expression.value, **asdict(vcl.MISSING))

    def translate_DimensionCons(self, expression: vcl.DimensionCons) -> py.expr:
        """Translate DimensionCons to builtin call."""
        return py_app(
            py_builtin("DimensionCons", provenance=vcl.MISSING),
            self.translate_expression(expression.e1),
            self.translate_expression(expression.e2),
            provenance=vcl.MISSING,
        )

    def translate_DimensionIndex(self, expression: vcl.DimensionIndex) -> py.expr:
        """Translate DimensionIndex to constant."""
        return py.Constant(value=expression.i, **asdict(vcl.MISSING))

    def translate_DimensionNil(self, expression: vcl.DimensionNil) -> py.expr:
        """Translate DimensionNil to empty tuple."""
        return py_tuple([], provenance=vcl.MISSING)

    def translate_ConstTensor(self, expression: vcl.ConstTensor) -> py.expr:
        """Translate ConstTensor to builtin call."""
        return py_app(
            py_builtin("ConstTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.c),
            self.translate_expression(expression.ds),
            provenance=vcl.MISSING,
        )

    def translate_StackTensor(self, expression: vcl.StackTensor) -> py.expr:
        """Translate StackTensor to builtin call."""
        return py_app(
            py_builtin("StackTensor", provenance=vcl.MISSING),
            py_tuple(
                [self.translate_expression(x) for x in expression.xs],
                provenance=vcl.MISSING,
            ),
            provenance=vcl.MISSING,
        )

    def translate_Transpose(self, expression: vcl.Transpose) -> py.expr:
        """Translate Transpose to builtin call."""
        return py_app(
            py_builtin("Transpose", provenance=vcl.MISSING),
            self.translate_expression(expression.xs),
            provenance=vcl.MISSING,
        )

    def translate_AtTensor(self, expression: vcl.AtTensor) -> py.expr:
        """Translate AtTensor to builtin call."""
        return py_app(
            py_builtin("AtTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.xs),
            self.translate_expression(expression.i),
            provenance=vcl.MISSING,
        )

    def translate_ForeachTensor(self, expression: vcl.ForeachTensor) -> py.expr:
        """Translate ForeachTensor to builtin call."""
        return py_app(
            py_builtin("ForeachTensor", provenance=vcl.MISSING),
            self.translate_expression(expression.size),
            self.translate_expression(expression.function),
            provenance=vcl.MISSING,
        )

    def translate_VectorLiteral(self, expression: vcl.VectorLiteral) -> py.expr:
        """Translate VectorLiteral to builtin call."""
        return py_app(
            py_builtin("VectorLiteral", provenance=vcl.MISSING),
            py_tuple(
                [self.translate_expression(x) for x in expression.elements],
                provenance=vcl.MISSING,
            ),
            provenance=vcl.MISSING,
        )

    def translate_AtVector(self, expression: vcl.AtVector) -> py.expr:
        """Translate AtVector to builtin call."""
        return py_app(
            py_builtin("AtVector", provenance=vcl.MISSING),
            self.translate_expression(expression.xs),
            self.translate_expression(expression.i),
            provenance=vcl.MISSING,
        )

    def translate_ForeachVector(self, expression: vcl.ForeachVector) -> py.expr:
        """Translate ForeachVector to builtin call."""
        return py_app(
            py_builtin("ForeachVector", provenance=vcl.MISSING),
            self.translate_expression(expression.size),
            self.translate_expression(expression.function),
            provenance=vcl.MISSING,
        )


def py_name(name: vcl.Name, *, provenance: vcl.Provenance) -> py.Name:
    """Make a name."""
    return py.Name(
        id=name,
        ctx=py.Load(),
        **asdict(provenance),
    )


def py_qualified_name(*parts: vcl.Name, provenance: vcl.Provenance) -> py.expr:
    """Make a qualified name."""
    if not parts:
        raise ValueError("A qualified name should have at least one part.")

    def py_attribute(value: py.expr, attr: str) -> py.expr:
        return py.Attribute(value=value, attr=attr, ctx=py.Load(), **asdict(provenance))

    initial: py.expr = py_name(parts[0], provenance=provenance)
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


def py_builtin(builtin: str, *, provenance: vcl.Provenance) -> py.expr:
    """Make a builtin function call."""
    return py_qualified_name("__vehicle__", builtin, provenance=provenance)


def py_subscript(
    value: py.expr, slice: py.expr, *, provenance: vcl.Provenance
) -> py.expr:
    """Make a subscript expression."""
    return py.Subscript(value=value, slice=slice, ctx=py.Load(), **asdict(provenance))


def py_app(
    function: py.expr, *arguments: py.expr, provenance: vcl.Provenance
) -> py.expr:
    """Make a function call: function(arguments[0],...,arguments[n])"""
    return py.Call(
        func=function,
        args=list(arguments),
        keywords=[],
        **asdict(provenance),
    )


def py_app_sequential(
    function: py.expr, arguments: Sequence[py.expr], provenance: vcl.Provenance
) -> py.expr:
    """Make a series of function calls: function(arguments[0])...(arguments[n])."""
    if not arguments:
        return function

    return py_app_sequential(
        function=py.Call(
            func=function,
            args=[arguments[0]],
            keywords=[],
            **asdict(provenance),
        ),
        arguments=arguments[1:],
        provenance=provenance,
    )


def py_fraction(value: Fraction, provenance: vcl.Provenance) -> py.expr:
    return py_app(
        py_qualified_name("fractions", "Fraction", provenance=provenance),
        py.Constant(
            value=value.numerator,
            **asdict(provenance),
        ),
        py.Constant(
            value=value.denominator,
            **asdict(provenance),
        ),
        provenance=provenance,
    )


def py_extended_fraction(
    value: vcl.ExtendedFraction, provenance: vcl.Provenance
) -> py.expr:
    match value:
        case vcl.Finite(value=inner):
            return py_fraction(inner, provenance=provenance)
        case vcl.PosInfinity():
            return py.Constant(value=float("inf"), **asdict(provenance))
        case vcl.NegInfinity():
            return py.Constant(value=float("-inf"), **asdict(provenance))
        case _:
            raise ValueError(f"Unknown extended rational type: {type(value)}")


def py_scalar(value: vcl.DType, provenance: vcl.Provenance) -> py.expr:
    """Make a scalar."""
    match value:
        case vcl.ExtendedFraction():
            return py_extended_fraction(value, provenance=provenance)
        case _:
            return py.Constant(
                value=value,
                **asdict(provenance),
            )


def py_tuple(elements: list[py.expr], provenance: vcl.Provenance) -> py.expr:
    """Make a tuple."""
    return py.Tuple(
        elts=list(elements),
        ctx=py.Load(),
        **asdict(provenance),
    )


def py_tensor(tensor: vcl.Tensor[vcl.DType], provenance: vcl.Provenance) -> py.expr:
    """Make a tensor by calling appropriate builtin."""
    match tensor:
        case vcl.DenseTensor():
            # DenseTensor: call __vehicle__.DenseTensor(values, shape)
            return py_app(
                py_builtin("DenseTensor", provenance=provenance),
                py_tuple(
                    [py_scalar(val, provenance=provenance) for val in tensor.values],
                    provenance=provenance,
                ),
                py_tuple(
                    [
                        py.Constant(value=dim, **asdict(provenance))
                        for dim in tensor.shape
                    ],
                    provenance=provenance,
                ),
                provenance=provenance,
            )
        case vcl.ConstantTensor():
            # ConstantTensor: call __vehicle__.ConstTensor(value, shape)
            return py_app(
                py_builtin("ConstTensor", provenance=provenance),
                py_scalar(tensor.value, provenance=provenance),
                py_tuple(
                    [
                        py.Constant(value=dim, **asdict(provenance))
                        for dim in tensor.shape
                    ],
                    provenance=provenance,
                ),
                provenance=provenance,
            )
        case _:
            raise ValueError(f"Unknown tensor type: {type(tensor)}")
