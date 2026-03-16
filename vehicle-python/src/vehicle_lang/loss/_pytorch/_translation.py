import ast as py
from dataclasses import dataclass

from .._python import PythonTranslation
from ._builtins import PyTorchBuiltins

# Create proper Python AST provenance (different from Vehicle provenance)
PY_MISSING = {"lineno": 0, "col_offset": 0}

################################################################################
### PyTorch Translation
################################################################################


@dataclass(frozen=True, init=False)
class PyTorchTranslation(PythonTranslation):
    def __init__(self) -> None:
        super().__init__(
            builtins=PyTorchBuiltins(),
            module_header=[
                py.Import(
                    names=[
                        py.alias(
                            name="torch",
                            asname=None,
                            lineno=0,
                            col_offset=0,
                        )
                    ],
                    lineno=0,
                    col_offset=0,
                )
            ],
        )
