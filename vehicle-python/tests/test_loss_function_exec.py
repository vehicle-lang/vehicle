from pathlib import Path
from typing import Any, Dict, Iterator

import pytest
from vehicle_lang import session
from vehicle_lang.loss_function.translation.python import PythonTranslation


def sampler_for_x(**ctx: Any) -> Iterator[float]:
    yield 0.0
    yield 1.0


@pytest.mark.parametrize(
    "specification_filename,input_declaration_context,output_declaration_context",
    [
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
        (
            "test_quantifier_all.vcl",
            {"sampler_for_x": sampler_for_x},
            {"quantifierForall": 0.0},
        ),
        (
            "test_quantifier_any.vcl",
            {"sampler_for_x": sampler_for_x},
            {"quantifierExists": 0.0},
        ),
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
    specification_filename: str,
    input_declaration_context: Dict[str, Any],
    output_declaration_context: Dict[str, Any],
) -> None:
    compiler = PythonTranslation()
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    module = session.load(specification_path)
    result = compiler.compile(
        module, str(specification_path), input_declaration_context
    )
    for key in output_declaration_context.keys():
        if output_declaration_context[key] is not ...:
            result_at_key = result[key]() if key in result else None
            assert output_declaration_context[key] == result_at_key
        else:
            assert key in result
