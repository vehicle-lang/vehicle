from pathlib import Path
from typing import Any, Callable, Dict, Union

import pytest

import vehicle_lang as vcl
import vehicle_lang.compile.python as vcl2py


def network_validate_output(output: Dict[str, Any]) -> None:
    network = lambda xs: (sum(xs),)
    assert "prop" in output
    assert output["prop"](network) == 0.0


@pytest.mark.parametrize(
    "specification_filename,validate_output",
    [
        (
            "test_addition.vcl",
            {"prop": 0.0},
        ),
        (
            "test_at.vcl",
            {"prop": 0.0},
        ),
        (
            "test_constant.vcl",
            {"prop": 0.0},
        ),
        (
            "test_division.vcl",
            {"prop": 0.0},
        ),
        (
            "test_indicator.vcl",
            {"prop": 1.0},
        ),
        (
            "test_maximum.vcl",
            {"prop": 3.5},
        ),
        (
            "test_minimum.vcl",
            {"prop": 0.0},
        ),
        (
            "test_multiplication.vcl",
            {"prop": 0.0},
        ),
        (
            "test_negation.vcl",
            {"prop": 0.0},
        ),
        (
            "test_network.vcl",
            network_validate_output,
        ),
        (
            "test_subtraction.vcl",
            {"prop": 0.0},
        ),
        (
            "test_tensor.vcl",
            {"prop": 0.0},
        ),
        (
            "test_variable.vcl",
            {"prop": 0.0},
        ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_filename: str,
    validate_output: Union[Dict[str, Any], Callable[[Dict[str, Any]], None]],
) -> None:
    print(f"Exec {specification_filename}")
    path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = vcl2py.load(path, target=vcl.DifferentiableLogic.DL2)
    if isinstance(validate_output, dict):
        for key in validate_output.keys():
            if validate_output[key] is not ...:
                assert validate_output[key] == actual_declarations.get(key, None)
            else:
                assert key in actual_declarations
    elif callable(validate_output):
        validate_output(actual_declarations)
