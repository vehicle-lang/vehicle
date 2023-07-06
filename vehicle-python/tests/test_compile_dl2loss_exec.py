from pathlib import Path
from typing import Any, Callable, Dict, Union

import pytest

from vehicle_lang.compile import Target, to_python


def validate_output_test_network(expected_declarations: Dict[str, Any]) -> None:
    net = lambda xs: (sum(xs),)
    assert "net_prop" in expected_declarations
    assert expected_declarations["net_prop"](net)


@pytest.mark.parametrize(
    "specification_filename,expected_declarations",
    [
        (
            "test_addition.vcl",
            {"addition": 8},
        ),
        (
            "test_at.vcl",
            {"at": 3.0},
        ),
        (
            "test_constant.vcl",
            {"constant": 5},
        ),
        (
            "test_division.vcl",
            {"division": 3.0},
        ),
        (
            "test_indicator.vcl",
            {"indicator": False},
        ),
        (
            "test_maximum.vcl",
            {"maximum": 4.0},
        ),
        (
            "test_minimum.vcl",
            {"minimum": 0.0},
        ),
        (
            "test_multiplication.vcl",
            {"multiplication": 12},
        ),
        (
            "test_negation.vcl",
            {"negation": -5},
        ),
        (
            "test_network.vcl",
            validate_output_test_network,
        ),
        (
            "test_subtraction.vcl",
            {"subtraction": 4},
        ),
        (
            "test_tensor.vcl",
            {"tensor": (5, 2, 16, 7)},
        ),
        (
            "test_variable.vcl",
            {"variable": 2},
        ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_filename: str,
    expected_declarations: Union[Dict[str, Any], Callable[[Dict[str, Any]], None]],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = to_python(specification_path, target=Target.LOSS_DL2)
    if isinstance(expected_declarations, dict):
        for key in expected_declarations.keys():
            if expected_declarations[key] is not ...:
                assert expected_declarations[key] == actual_declarations.get(key, None)
            else:
                assert key in actual_declarations
    elif callable(expected_declarations):
        expected_declarations(actual_declarations)
