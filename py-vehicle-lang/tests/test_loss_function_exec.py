from pathlib import Path
from typing import Any, Dict, List, Tuple

import pytest
from typing_extensions import TypeAlias
from vehicle_lang import session
from vehicle_lang.loss_function.translation.python import PythonTranslation


@pytest.mark.parametrize(
    "specification_name,input_declaration_context,output_declaration_context",
    [
        (
            "test_one.vcl",
            {},
            {"one": 1},
        ),
        (
            "test_two.vcl",
            {"one": 1},
            {"two": 2},
        ),
        (
            "test_addition.vcl",
            {},
            {"addition": 8},
        ),
        # TODO: vehicle: NonEmpty.fromList: empty list
        # (
        #     "test_at.vcl",
        #     {},
        #     {"at": 2},
        # ),
        (
            "test_constant.vcl",
            {},
            {"constant": 5},
        ),
        # TODO: encountered unexpected expression 'Application of lambda functions is not handled at the moment for loss function translation.' during compilation to loss functions.
        # (
        #     "test_division.vcl",
        #     {},
        #     {"division": 3},
        # ),
        (
            "test_indicator.vcl",
            {},
            {"indicator": 1},
        ),
        (
            "test_maximum.vcl",
            {},
            {"maximum": 4},
        ),
        (
            "test_minimum.vcl",
            {},
            {"minimum": 0},
        ),
        (
            "test_multiplication.vcl",
            {},
            {"multiplication": 12},
        ),
        (
            "test_negation.vcl",
            {},
            {"negation": -5},
        ),
        (
            "test_network.vcl",
            {"net": lambda inputs: [sum(inputs)]},
            {"net_prop": 0},
        ),
        # (
        #     "test_quantifier_all.vcl",
        #     {},
        #     {"quantifierForall": ...},
        # ),
        # (
        #     "test_quantifier_any.vcl",
        #     {},
        #     {"quantifierExists": ...},
        # ),
        (
            "test_subtraction.vcl",
            {},
            {"subtraction": 4},
        ),
        (
            "test_tensor.vcl",
            {},
            {"tensor": [5, 2, 16, 7]},
        ),
        # TODO: vehicle: NonEmpty.fromList: empty list
        # (
        #     "test_variable.vcl",
        #     {},
        #     {"variable": 2},
        # ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_name: str,
    input_declaration_context: Dict[str, Any],
    output_declaration_context: Dict[str, Any],
) -> None:
    specification_path = Path(__file__).parent / "data" / specification_name
    compiler = PythonTranslation()
    module = session.load(specification_path)
    result = compiler.compile(module, specification_name, input_declaration_context)
    for key in output_declaration_context.keys():
        if output_declaration_context[key] is not ...:
            assert output_declaration_context[key] == result.get(key)
