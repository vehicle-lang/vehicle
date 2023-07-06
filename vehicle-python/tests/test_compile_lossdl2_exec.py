from pathlib import Path
from typing import Any, Dict

import pytest

from vehicle_lang.compile import Target, to_python


@pytest.mark.parametrize(
    "specification_filename,golden_declarations",
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
            {"maximum": False},
        ),
        (
            "test_minimum.vcl",
            {"minimum": True},
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
            {"net_prop": ...},
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
    golden_declarations: Dict[str, Any],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = to_python(specification_path, target=Target.LOSS_DL2)
    print(repr(actual_declarations))
    for key in golden_declarations.keys():
        if golden_declarations[key] is not ...:
            actual_declarations_at_key = (
                actual_declarations[key] if key in actual_declarations else None
            )
            assert golden_declarations[key] == actual_declarations_at_key
        else:
            assert key in actual_declarations
