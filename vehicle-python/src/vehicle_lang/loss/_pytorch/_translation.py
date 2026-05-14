import ast as py
from dataclasses import dataclass
from typing import Any

from .._python import PythonTranslation
from ._builtins import PyTorchBuiltins

# Create proper Python AST provenance (different from Vehicle provenance)
PY_MISSING = {"lineno": 0, "col_offset": 0}

################################################################################
### PyTorch Translation
################################################################################


@dataclass(frozen=True, init=False)
class PyTorchTranslation(PythonTranslation):
    def __init__(self, *, temporal_semantics: Any | None = None) -> None:
        super().__init__(
            builtins=PyTorchBuiltins(temporal_semantics=temporal_semantics),
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
