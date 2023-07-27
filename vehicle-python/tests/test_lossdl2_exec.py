from pathlib import Path
from typing import Any, Callable, Dict, Iterator, Union

import pytest

from vehicle_lang import DifferentiableLogic, generate_loss_function


def network_validate_output(output: Dict[str, Any]) -> None:
    network = lambda xs: (sum(xs),)
    assert "prop" in output
    assert output["prop"](network) == 0.0


def quantifier_all_sampler(context: Dict[str, Any]) -> Iterator[Any]:
    yield from [-10.0, -1.0, 1.0, 10.0]


def quantifier_any_sampler(context: Dict[str, Any]) -> Iterator[Any]:
    yield from [-10.0, -1.0, 1.0, 10.0]


@pytest.mark.parametrize(
    "specification_filename,samplers,validate_output",
    [
        (
            "test_addition.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_at.vcl",
            {},
            {"prop": 1.0},
        ),
        (
            "test_constant.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_division.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_indicator.vcl",
            {},
            {"prop": 1.0},
        ),
        (
            "test_maximum.vcl",
            {},
            {"prop": 1.0},
        ),
        (
            "test_minimum.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_multiplication.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_negation.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_network.vcl",
            {},
            network_validate_output,
        ),
        (
            "test_quantifier_all.vcl",
            {"x": quantifier_all_sampler},
            {"prop": 11.0},
        ),
        (
            "test_quantifier_any.vcl",
            {"x": quantifier_any_sampler},
            {"prop": 0.0},
        ),
        (
            "test_subtraction.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_tensor.vcl",
            {},
            {"prop": 4.0},
        ),
        (
            "test_variable.vcl",
            {},
            {"prop": 0.0},
        ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_filename: str,
    samplers: Dict[str, Any],
    validate_output: Union[Dict[str, Any], Callable[[Dict[str, Any]], None]],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = generate_loss_function(
        specification_path,
        differentiable_logic=DifferentiableLogic.DL2,
        samplers=samplers,
    )
    if isinstance(validate_output, dict):
        for key in validate_output.keys():
            if validate_output[key] is not ...:
                assert validate_output[key] == actual_declarations.get(key, None)
            else:
                assert key in actual_declarations
    elif callable(validate_output):
        validate_output(actual_declarations)
