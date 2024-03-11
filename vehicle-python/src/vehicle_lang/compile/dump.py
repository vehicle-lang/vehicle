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
