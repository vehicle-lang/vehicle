import ast as py
from dataclasses import asdict, dataclass
from typing import Sequence

from typing_extensions import override

from .._ast import TensorLiteral
from .python import PythonTranslation


@dataclass(frozen=True)
class TensorflowTranslation(PythonTranslation):
    @override
    def get_module_header(self) -> Sequence[py.stmt]:
        return [py.Import(names=py.alias("tensorflow"))]

    @override
    def translate_TensorLiteral(self, expression: TensorLiteral) -> py.expr:
        provenance = asdict(expression.provenance)
        tensorflow_name = py.Name("tensorflow", py.Load(), **provenance)
        convert_to_name = py.Attribute(
            tensorflow_name, "convert_to_tensor", py.Load(), **provenance
        )
        py_list_literal = super().translate_TensorLiteral(expression)
        return py.Call(convert_to_name, [py_list_literal], [], **provenance)
