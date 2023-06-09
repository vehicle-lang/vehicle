from pathlib import Path
from typing import Any, Dict, Union

import pytest
from typing_extensions import TypeAlias
from vehicle_lang import session
from vehicle_lang.loss_function import Module
from vehicle_lang.loss_function.translation.python import PythonTranslation

TEST_DATA_PATH = Path(__file__).parent / "data"


@pytest.mark.parametrize(
    "specification_path,input_declaration_context,output_declaration_context",
    [
        (
            TEST_DATA_PATH / "test_one.vcl",
            {},
            {"one": 1},
        ),
        (
            TEST_DATA_PATH / "test_two.vcl",
            {"one": 1},
            {"two": 2},
        ),
        (
            TEST_DATA_PATH / "test_addition.vcl",
            {},
            {"addition": 8},
        ),
        # TODO: vehicle: NonEmpty.fromList: empty list
        # (
        #     TEST_DATA_PATH / "test_at.vcl",
        #     {},
        #     {"at": 2},
        # ),
        (
            TEST_DATA_PATH / "test_constant.vcl",
            {},
            {"constant": 5},
        ),
        # TODO: encountered unexpected expression 'Application of lambda functions is not handled at the moment for loss function translation.' during compilation to loss functions.
        # (
        #     TEST_DATA_PATH / "test_division.vcl",
        #     {},
        #     {"division": 3},
        # ),
        (
            TEST_DATA_PATH / "test_indicator.vcl",
            {},
            {"indicator": 1},
        ),
        (
            TEST_DATA_PATH / "test_maximum.vcl",
            {},
            {"maximum": 4},
        ),
        (
            TEST_DATA_PATH / "test_minimum.vcl",
            {},
            {"minimum": 0},
        ),
        (
            TEST_DATA_PATH / "test_multiplication.vcl",
            {},
            {"multiplication": 12},
        ),
        (
            TEST_DATA_PATH / "test_negation.vcl",
            {},
            {"negation": -5},
        ),
        (
            TEST_DATA_PATH / "test_network.vcl",
            {"net": lambda inputs: [sum(inputs)]},
            {"net_prop": 0},
        ),
        # (
        #     TEST_DATA_PATH / "test_quantifier_all.vcl",
        #     {},
        #     {"quantifierForall": ...},
        # ),
        # (
        #     TEST_DATA_PATH / "test_quantifier_any.vcl",
        #     {},
        #     {"quantifierExists": ...},
        # ),
        (
            TEST_DATA_PATH / "test_subtraction.vcl",
            {},
            {"subtraction": 4},
        ),
        (
            TEST_DATA_PATH / "test_tensor.vcl",
            {},
            {"tensor": [5, 2, 16, 7]},
        ),
        # TODO: vehicle: NonEmpty.fromList: empty list
        # (
        #     TEST_DATA_PATH / "test_variable.vcl",
        #     {},
        #     {"variable": 2},
        # ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_path: Path,
    input_declaration_context: Dict[str, Any],
    output_declaration_context: Dict[str, Any],
) -> None:
    compiler = PythonTranslation()
    module = session.load(specification_path)
    result = compiler.compile(
        module, specification_path.name, input_declaration_context
    )
    for key in output_declaration_context.keys():
        if output_declaration_context[key] is not ...:
            assert output_declaration_context[key] == result.get(key)
