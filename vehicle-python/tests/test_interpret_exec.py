from pathlib import Path
from typing import Any, Dict

import pytest

from vehicle_lang.exec import compile
from vehicle_lang.exec.builtin.python import PythonBuiltins
from vehicle_lang.exec.translation.python import PythonTranslation


@pytest.mark.parametrize(
    "specification_filename,input_declaration_context,output_declaration_context",
    [
        (
            "test_addition.vcl",
            {},
            {"addition": 8},
        ),
        # (
        #     "test_at.vcl",
        #     {},
        #     {"at": 2},
        # ),
        # (
        #     "test_constant.vcl",
        #     {},
        #     {"constant": 5},
        # ),
        # (
        #     "test_division.vcl",
        #     {},
        #     {"division": 3},
        # ),
        # (
        #     "test_indicator.vcl",
        #     {},
        #     {"indicator": 1},
        # ),
        # (
        #     "test_maximum.vcl",
        #     {},
        #     {"maximum": 4},
        # ),
        # (
        #     "test_minimum.vcl",
        #     {},
        #     {"minimum": 0},
        # ),
        # (
        #     "test_multiplication.vcl",
        #     {},
        #     {"multiplication": 12},
        # ),
        # (
        #     "test_negation.vcl",
        #     {},
        #     {"negation": -5},
        # ),
        # (
        #     "test_network.vcl",
        #     {"net": lambda inputs: [sum(inputs)]},
        #     {"net_prop": 0},
        # ),
        # (
        #     "test_subtraction.vcl",
        #     {},
        #     {"subtraction": 4},
        # ),
        # (
        #     "test_tensor.vcl",
        #     {},
        #     {"tensor": [5, 2, 16, 7]},
        # ),
        # (
        #     "test_variable.vcl",
        #     {},
        #     {"variable": 2},
        # ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_filename: str,
    input_declaration_context: Dict[str, Any],
    output_declaration_context: Dict[str, Any],
) -> None:
    builtins = PythonBuiltins()
    translation = PythonTranslation(builtins=builtins)
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    result = compile.to_python(
        specification_path=str(specification_path),
        declaration_context=input_declaration_context,
        translation=translation,
    )
    for key in output_declaration_context.keys():
        if output_declaration_context[key] is not ...:
            result_at_key = result[key] if key in result else None
            assert output_declaration_context[key] == result_at_key
        else:
            assert key in result
