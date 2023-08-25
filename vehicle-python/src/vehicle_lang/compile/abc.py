import itertools
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from functools import reduce
from typing import Any, Callable, Dict, Generic, SupportsFloat, SupportsInt, Tuple

from typing_extensions import TypeAlias, TypeVar, override

from .. import ast as vcl
from ..typing import Optimiser
from . import _numeric
from ._collections import SupportsList, SupportsVector
from .error import VehicleBuiltinUnsupported

################################################################################
### Interpretations of Vehicle builtins in Python
################################################################################

_SupportsNat = TypeVar("_SupportsNat", bound=_numeric.SupportsNat)
_SupportsInt = TypeVar("_SupportsInt", bound=_numeric.SupportsInt)
_SupportsRat = TypeVar("_SupportsRat", bound=_numeric.SupportsRat)

_S = TypeVar("_S")
_T = TypeVar("_T")
_U = TypeVar("_U")


@dataclass(frozen=True, init=False)
class ABCBuiltins(
    Generic[
        _SupportsNat,
        _SupportsInt,
        _SupportsRat,
    ],
    metaclass=ABCMeta,
):
    optimisers: Dict[str, Optimiser[Any, _SupportsRat]] = field(default_factory=dict)

    def AddInt(self, x: _SupportsInt, y: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x + y

    def AddNat(self, x: _SupportsNat, y: _SupportsNat) -> _SupportsNat:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x + y

    def AddRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x + y

    def And(self, x: bool, y: bool) -> bool:
        assert isinstance(x, bool), f"Expected bool, found {x}"
        assert isinstance(y, bool), f"Expected bool, found {y}"
        return x and y

    @abstractmethod
    def AtVector(self, vector: SupportsVector[_T], index: int) -> _T:
        ...

    def Bool(self, value: bool) -> bool:
        assert isinstance(value, bool), f"Expected bool, found {value}"
        return bool(value)

    def ConsList(self, item: _T, iterable: SupportsList[_T]) -> SupportsList[_T]:
        assert isinstance(iterable, SupportsList), f"Expected list, found {iterable}"
        return itertools.chain((item,), iterable)

    def DivRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x / y

    def EqIndex(self, x: int, y: int) -> bool:
        assert isinstance(x, int), f"Expected int, found {x}"
        assert isinstance(y, int), f"Expected int, found {y}"
        return x == y

    def EqInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x == y

    def EqNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x == y

    def EqRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x == y

    def Exists(
        self, name: str, context: Dict[str, Any], predicate: Callable[[_T], bool]
    ) -> bool:
        raise VehicleBuiltinUnsupported(vcl.Exists.__name__)

    def FoldList(
        self, function: Callable[[_S, _T], _T], initial: _T, iterable: SupportsList[_S]
    ) -> _T:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(iterable, SupportsList), f"Expected list, found {iterable}"
        return reduce(lambda x, y: function(y, x), iterable, initial)

    @abstractmethod
    def FoldVector(
        self, function: Callable[[_S, _T], _T], initial: _T, vector: SupportsVector[_S]
    ) -> _T:
        ...

    def Forall(
        self, name: str, context: Dict[str, Any], predicate: Callable[[_T], bool]
    ) -> bool:
        raise VehicleBuiltinUnsupported(vcl.Forall.__name__)

    def GeIndex(self, x: int, y: int) -> bool:
        assert isinstance(x, int), f"Expected int, found {x}"
        assert isinstance(y, int), f"Expected int, found {y}"
        return x >= y

    def GeInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x >= y

    def GeNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x >= y

    def GeRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x >= y

    def GtIndex(self, x: int, y: int) -> bool:
        assert isinstance(x, int), f"Expected int, found {x}"
        assert isinstance(y, int), f"Expected int, found {y}"
        return x > y

    def GtInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x > y

    def GtNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x > y

    def GtRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x > y

    def If(self, cond: bool, if_true: _T, if_false: _T) -> _T:
        assert isinstance(cond, bool), f"Expected bool, found {cond}"
        return if_true if cond else if_false

    def Implies(self, x: bool, y: bool) -> bool:
        return self.Or(self.Not(x), y)

    def Index(self, value: SupportsInt) -> int:
        return value.__int__()

    def Indices(self, upto: int) -> SupportsVector[int]:
        assert isinstance(upto, int), f"Expected int, found {upto}"
        return self.Vector(*range(0, upto))

    @abstractmethod
    def Int(self, value: SupportsInt) -> _SupportsInt:
        ...

    def LeIndex(self, x: int, y: int) -> bool:
        assert isinstance(x, int), f"Expected int, found {x}"
        assert isinstance(y, int), f"Expected int, found {y}"
        return x <= y

    def LeInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x <= y

    def LeNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x <= y

    def LeRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x <= y

    def LtIndex(self, x: int, y: int) -> bool:
        assert isinstance(x, int), f"Expected int, found {x}"
        assert isinstance(y, int), f"Expected int, found {y}"
        return x < y

    def LtInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x < y

    def LtNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x < y

    def LtRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x < y

    def MapList(
        self, function: Callable[[_S], _T], iterable: SupportsList[_S]
    ) -> SupportsList[_T]:
        assert callable(function), f"Expected function, found {function}"
        assert isinstance(iterable, SupportsList), f"Expected list, found {iterable}"
        return map(function, iterable)

    @abstractmethod
    def MapVector(
        self, function: Callable[[_S], _T], vector: SupportsVector[_S]
    ) -> SupportsVector[_T]:
        ...

    def MaxRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return max(x, y)

    def MinRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return min(x, y)

    def MulInt(self, x: _SupportsInt, y: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x * y

    def MulNat(self, x: _SupportsNat, y: _SupportsNat) -> _SupportsNat:
        assert isinstance(x, _numeric.SupportsNat), f"Expected Nat, found {x}"
        assert isinstance(y, _numeric.SupportsNat), f"Expected Nat, found {y}"
        return x * y

    def MulRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x * y

    @abstractmethod
    def Nat(self, value: SupportsInt) -> _SupportsNat:
        ...

    def NeIndex(self, x: int, y: int) -> bool:
        return self.Not(self.EqIndex(x, y))

    def NeInt(self, x: _SupportsInt, y: _SupportsInt) -> bool:
        return self.Not(self.EqInt(x, y))

    def NeNat(self, x: _SupportsNat, y: _SupportsNat) -> bool:
        return self.Not(self.EqNat(x, y))

    def NeRat(self, x: _SupportsRat, y: _SupportsRat) -> bool:
        return self.Not(self.EqRat(x, y))

    def NegInt(self, x: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        return -x

    def NegRat(self, x: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        return -x

    def NilList(self) -> SupportsList[_T]:
        return ()

    def Not(self, x: bool) -> bool:
        assert isinstance(x, bool), f"Expected bool, found {x}"
        return not x

    def Optimise(
        self,
        name: str,
        minimise: bool,
        context: Dict[str, Any],
        joiner: Callable[[Any, Any], Any],
        predicate: Callable[[Any], Any],
    ) -> Any:
        if name in self.optimisers:
            return self.optimisers[name](minimise, context, joiner, predicate)
        else:
            raise TypeError(f"Could not find optimiser for '{name}'.")

    def Or(self, x: bool, y: bool) -> bool:
        assert isinstance(x, bool), f"Expected bool, found {x}"
        assert isinstance(y, bool), f"Expected bool, found {y}"
        return x or y

    def PowRat(self, x: _SupportsRat, y: _SupportsInt) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x ** y.__int__()

    @abstractmethod
    def Rat(self, value: SupportsFloat) -> _SupportsRat:
        ...

    def SubInt(self, x: _SupportsInt, y: _SupportsInt) -> _SupportsInt:
        assert isinstance(x, _numeric.SupportsInt), f"Expected Int, found {x}"
        assert isinstance(y, _numeric.SupportsInt), f"Expected Int, found {y}"
        return x - y

    def SubRat(self, x: _SupportsRat, y: _SupportsRat) -> _SupportsRat:
        assert isinstance(x, _numeric.SupportsRat), f"Expected Rat, found {x}"
        assert isinstance(y, _numeric.SupportsRat), f"Expected Rat, found {y}"
        return x - y

    def Unit(self) -> Tuple[()]:
        return ()

    @abstractmethod
    def Vector(self, *values: _T) -> SupportsVector[_T]:
        ...

    @abstractmethod
    def ZipWithVector(
        self,
        function: Callable[[_S, _T], _U],
        vector1: SupportsVector[_S],
        vector2: SupportsVector[_T],
    ) -> SupportsVector[_U]:
        ...


AnyBuiltins: TypeAlias = ABCBuiltins[Any, Any, Any]

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
