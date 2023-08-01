import itertools
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from functools import reduce
from typing import Any, Callable, Dict, Generic, SupportsFloat, SupportsInt, Tuple

from typing_extensions import TypeAlias, TypeVar, override

from .. import ast as vcl
from ..error import BuiltinUnsupported
from ..typing import Sampler
from ._collections import SupportsList, SupportsVector

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

_Bool = TypeVar("_Bool")
_Nat = TypeVar("_Nat")
_Int = TypeVar("_Int")
_Rat = TypeVar("_Rat")

_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")


@dataclass(frozen=True, init=False)
class Builtins(
    Generic[
        _Bool,
        _Nat,
        _Int,
        _Rat,
    ],
    metaclass=ABCMeta,
):
    samplers: Dict[str, Sampler[Any]] = field(default_factory=dict)

    @abstractmethod
    def AddInt(self, x: _Int, y: _Int) -> _Int:
        ...

    @abstractmethod
    def AddNat(self, x: _Nat, y: _Nat) -> _Nat:
        ...

    @abstractmethod
    def AddRat(self, x: _Rat, y: _Rat) -> _Rat:
        ...

    @abstractmethod
    def And(self, x: _Bool, y: _Bool) -> _Bool:
        ...

    def AtVector(self, vector: SupportsVector[_T], index: int) -> _T:
        assert isinstance(vector, SupportsVector), f"Expected vector, found {vector}"
        assert isinstance(index, int), f"Expected int, found {vector}"
        return vector[index]

    @abstractmethod
    def Bool(self, value: bool) -> _Bool:
        ...

    def ConsList(self, item: _T, iterable: SupportsList[_T]) -> SupportsList[_T]:
        assert isinstance(iterable, SupportsList), f"Expected list, found {iterable}"
        return itertools.chain((item,), iterable)

    def ConsVector(self, item: _T, vector: SupportsVector[_T]) -> SupportsVector[_T]:
        return (item, *vector)

    @abstractmethod
    def DivRat(self, x: _Rat, y: _Rat) -> _Rat:
        ...

    @abstractmethod
    def EqIndex(self, x: int, y: int) -> _Bool:
        ...

    @abstractmethod
    def EqInt(self, x: _Int, y: _Int) -> _Bool:
        ...

    @abstractmethod
    def EqNat(self, x: _Nat, y: _Nat) -> _Bool:
        ...

    @abstractmethod
    def EqRat(self, x: _Rat, y: _Rat) -> _Bool:
        ...

    def Exists(
        self, name: str, context: Dict[str, Any], predicate: Callable[[_T], _Bool]
    ) -> _Bool:
        raise BuiltinUnsupported(vcl.Exists.__name__)

    def FoldList(
        self, function: Callable[[_S, _T], _T], initial: _T, iterable: SupportsList[_S]
    ) -> _T:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(iterable, SupportsList), f"Expected list, found {iterable}"
        return reduce(lambda x, y: function(y, x), iterable, initial)

    def FoldVector(
        self, function: Callable[[_S, _T], _T], initial: _T, vector: SupportsVector[_S]
    ) -> _T:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(vector, SupportsVector), f"Expected vector, found {vector}"
        return reduce(lambda x, y: function(y, x), vector, initial)

    def Forall(
        self, name: str, context: Dict[str, Any], predicate: Callable[[_T], _Bool]
    ) -> _Bool:
        raise BuiltinUnsupported(vcl.Forall.__name__)

    def GeIndex(self, x: int, y: int) -> _Bool:
        return self.Not(self.LtIndex(x, y))

    def GeInt(self, x: _Int, y: _Int) -> _Bool:
        return self.Not(self.LtInt(x, y))

    def GeNat(self, x: _Nat, y: _Nat) -> _Bool:
        return self.Not(self.LtNat(x, y))

    def GeRat(self, x: _Rat, y: _Rat) -> _Bool:
        return self.Not(self.LtRat(x, y))

    def GtIndex(self, x: int, y: int) -> _Bool:
        return self.Not(self.LeIndex(x, y))

    def GtInt(self, x: _Int, y: _Int) -> _Bool:
        return self.Not(self.LeInt(x, y))

    def GtNat(self, x: _Nat, y: _Nat) -> _Bool:
        return self.Not(self.LeNat(x, y))

    def GtRat(self, x: _Rat, y: _Rat) -> _Bool:
        return self.Not(self.LeRat(x, y))

    @abstractmethod
    def If(self, cond: _Bool, if_true: _T, if_false: _T) -> _T:
        ...

    def Implies(self, x: _Bool, y: _Bool) -> _Bool:
        return self.Or(self.Not(x), y)

    def Index(self, value: SupportsInt) -> int:
        return value.__int__()

    def Indices(self, upto: int) -> SupportsVector[int]:
        assert isinstance(upto, int), f"Expected int, found {upto}"
        return tuple(range(0, upto))

    @abstractmethod
    def Int(self, value: SupportsInt) -> _Int:
        ...

    def LeIndex(self, x: int, y: int) -> _Bool:
        return self.Or(self.EqIndex(x, y), self.LtIndex(x, y))

    def LeInt(self, x: _Int, y: _Int) -> _Bool:
        return self.Or(self.EqInt(x, y), self.LtInt(x, y))

    def LeNat(self, x: _Nat, y: _Nat) -> _Bool:
        return self.Or(self.EqNat(x, y), self.LtNat(x, y))

    def LeRat(self, x: _Rat, y: _Rat) -> _Bool:
        return self.Or(self.EqRat(x, y), self.LtRat(x, y))

    @abstractmethod
    def LtIndex(self, x: int, y: int) -> _Bool:
        ...

    @abstractmethod
    def LtInt(self, x: _Int, y: _Int) -> _Bool:
        ...

    @abstractmethod
    def LtNat(self, x: _Nat, y: _Nat) -> _Bool:
        ...

    @abstractmethod
    def LtRat(self, x: _Rat, y: _Rat) -> _Bool:
        ...

    def MapList(
        self, function: Callable[[_S], _T], iterable: SupportsList[_S]
    ) -> SupportsList[_T]:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(iterable, SupportsList), f"Expected list, found {iterable}"
        return map(function, iterable)

    def MapVector(
        self, function: Callable[[_S], _T], vector: SupportsVector[_S]
    ) -> SupportsVector[_T]:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(vector, SupportsVector), f"Expected vector, found {vector}"
        return tuple(map(function, vector))

    @abstractmethod
    def MaxRat(self, x: _Rat, y: _Rat) -> _Rat:
        ...

    @abstractmethod
    def MinRat(self, x: _Rat, y: _Rat) -> _Rat:
        ...

    @abstractmethod
    def MulInt(self, x: _Int, y: _Int) -> _Int:
        ...

    @abstractmethod
    def MulNat(self, x: _Nat, y: _Nat) -> _Nat:
        ...

    @abstractmethod
    def MulRat(self, x: _Rat, y: _Rat) -> _Rat:
        ...

    @abstractmethod
    def Nat(self, value: SupportsInt) -> _Nat:
        ...

    def NeIndex(self, x: int, y: int) -> _Bool:
        return self.Not(self.EqIndex(x, y))

    def NeInt(self, x: _Int, y: _Int) -> _Bool:
        return self.Not(self.EqInt(x, y))

    def NeNat(self, x: _Nat, y: _Nat) -> _Bool:
        return self.Not(self.EqNat(x, y))

    def NeRat(self, x: _Rat, y: _Rat) -> _Bool:
        return self.Not(self.EqRat(x, y))

    @abstractmethod
    def NegInt(self, x: _Int) -> _Int:
        ...

    @abstractmethod
    def NegRat(self, x: _Rat) -> _Rat:
        ...

    def NilList(self) -> SupportsVector[_T]:
        return ()

    @abstractmethod
    def Not(self, x: _Bool) -> _Bool:
        ...

    @abstractmethod
    def Or(self, x: _Bool, y: _Bool) -> _Bool:
        ...

    @abstractmethod
    def PowRat(self, x: _Rat, y: _Int) -> _Rat:
        ...

    @abstractmethod
    def Rat(self, value: SupportsFloat) -> _Rat:
        ...

    def Sample(
        self,
        name: str,
        # predicate: Callable[[_T], _Bool],
        context: Dict[str, Any],
    ) -> SupportsList[_T]:
        if name in self.samplers:
            return tuple(self.samplers[name](context))
        else:
            raise TypeError(f"Could not find sampler for '{name}'.")

    @abstractmethod
    def SubInt(self, x: _Int, y: _Int) -> _Int:
        ...

    @abstractmethod
    def SubRat(self, x: _Rat, y: _Rat) -> _Rat:
        ...

    def Unit(self) -> Tuple[()]:
        return ()

    def ZipWithVector(
        self,
        function: Callable[[_S, _T], _U],
        vector1: SupportsVector[_S],
        vector2: SupportsVector[_T],
    ) -> SupportsVector[_U]:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(vector1, SupportsVector), f"Expected vector, found {vector1}"
        assert isinstance(vector2, SupportsVector), f"Expected vector, found {vector2}"
        return tuple(map(function, vector1, vector2))

    # NOTE: The definition for `Vector` must be last, otherwise mypy confuses
    #       it with the generic type `Vector`.

    def Vector(self, *values: _T) -> SupportsVector[_T]:
        return values


AnyBuiltins: TypeAlias = Builtins[Any, Any, Any, Any]

################################################################################
### Translation from Vehicle AST to Python AST
################################################################################


_Program = TypeVar("_Program")
_Declaration = TypeVar("_Declaration")
_Expression = TypeVar("_Expression")


class Translation(Generic[_Program, _Declaration, _Expression], metaclass=ABCMeta):
    @abstractmethod
    def translate_program(self, program: vcl.Program) -> _Program:
        ...

    @abstractmethod
    def translate_declaration(self, declaration: vcl.Declaration) -> _Declaration:
        ...

    @abstractmethod
    def translate_expression(self, expression: vcl.Expression) -> _Expression:
        ...


class ABCTranslation(Translation[_Program, _Declaration, _Expression]):
    @override
    def translate_program(self, program: vcl.Program) -> _Program:
        if isinstance(program, vcl.Main):
            return self.translate_Main(program)
        raise NotImplementedError(type(program).__name__)

    @abstractmethod
    def translate_Main(self, program: vcl.Main) -> _Program:
        ...

    @override
    def translate_declaration(self, declaration: vcl.Declaration) -> _Declaration:
        if isinstance(declaration, vcl.DefFunction):
            return self.translate_DefFunction(declaration)
        if isinstance(declaration, vcl.DefPostulate):
            return self.translate_DefPostulate(declaration)
        raise NotImplementedError(type(declaration).__name__)

    @abstractmethod
    def translate_DefFunction(self, declaration: vcl.DefFunction) -> _Declaration:
        ...

    @abstractmethod
    def translate_DefPostulate(self, declaration: vcl.DefPostulate) -> _Declaration:
        ...

    @override
    def translate_expression(self, expression: vcl.Expression) -> _Expression:
        if isinstance(expression, vcl.App):
            return self.translate_App(expression)
        if isinstance(expression, vcl.BoundVar):
            return self.translate_BoundVar(expression)
        if isinstance(expression, vcl.Builtin):
            return self.translate_Builtin(expression)
        if isinstance(expression, vcl.FreeVar):
            return self.translate_FreeVar(expression)
        if isinstance(expression, vcl.Lam):
            return self.translate_Lam(expression)
        if isinstance(expression, vcl.Let):
            return self.translate_Let(expression)
        if isinstance(expression, vcl.PartialApp):
            return self.translate_PartialApp(expression)
        if isinstance(expression, vcl.Pi):
            return self.translate_Pi(expression)
        if isinstance(expression, vcl.Universe):
            return self.translate_Universe(expression)
        raise NotImplementedError(type(expression).__name__)

    @abstractmethod
    def translate_App(self, expression: vcl.App) -> _Expression:
        ...

    @abstractmethod
    def translate_BoundVar(self, expression: vcl.BoundVar) -> _Expression:
        ...

    @abstractmethod
    def translate_Builtin(self, expression: vcl.Builtin) -> _Expression:
        ...

    @abstractmethod
    def translate_FreeVar(self, expression: vcl.FreeVar) -> _Expression:
        ...

    @abstractmethod
    def translate_Lam(self, expression: vcl.Lam) -> _Expression:
        ...

    def translate_Let(self, expression: vcl.Let) -> _Expression:
        return self.translate_expression(
            vcl.App(
                provenance=expression.provenance,
                function=vcl.Lam(
                    provenance=expression.provenance,
                    binders=(expression.binder,),
                    body=expression.body,
                ),
                arguments=[expression.bound],
            )
        )

    @abstractmethod
    def translate_PartialApp(self, expression: vcl.PartialApp) -> _Expression:
        ...

    @abstractmethod
    def translate_Pi(self, expression: vcl.Pi) -> _Expression:
        ...

    @abstractmethod
    def translate_Universe(self, expression: vcl.Universe) -> _Expression:
        ...
