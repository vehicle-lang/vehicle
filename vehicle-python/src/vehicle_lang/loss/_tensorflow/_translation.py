import ast as py
from dataclasses import dataclass

from .._python import PythonTranslation
from ._builtins import TensorFlowBuiltins

# Create proper Python AST provenance (different from Vehicle provenance)
PY_MISSING = {"lineno": 0, "col_offset": 0}

################################################################################
### TensorFlow Translation
################################################################################


@dataclass(frozen=True, init=False)
class TensorFlowTranslation(PythonTranslation):
    def __init__(self) -> None:
        super().__init__(
            builtins=TensorFlowBuiltins(),
            module_header=[
                py.Import(
                    names=[
                        py.alias(
                            name="tensorflow",
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
