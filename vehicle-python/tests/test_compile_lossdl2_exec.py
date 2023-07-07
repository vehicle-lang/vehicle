from pathlib import Path
from typing import Any, Callable, Dict, Iterator, Union

import pytest

from vehicle_lang.compile import Target, to_python


def network_validate_output(output: Dict[str, Any]) -> None:
    net = lambda xs: (sum(xs),)
    assert "net_prop" in output
    assert output["net_prop"](net) == 0.0


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
            {"addition": 8},
        ),
        (
            "test_at.vcl",
            {},
            {"at": 3.0},
        ),
        (
            "test_constant.vcl",
            {},
            {"constant": 5},
        ),
        (
            "test_division.vcl",
            {},
            {"division": 3.0},
        ),
        (
            "test_indicator.vcl",
            {},
            {"indicator": 0.0},
        ),
        (
            "test_maximum.vcl",
            {},
            {"maximum": 4.0},
        ),
        (
            "test_minimum.vcl",
            {},
            {"minimum": 0.0},
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
            {},
            network_validate_output,
        ),
        (
            "test_quantifier_all.vcl",
            {"x": quantifier_all_sampler},
            {"quantifierForall": False},
        ),
        (
            "test_quantifier_any.vcl",
            {"x": quantifier_any_sampler},
            {"quantifierExists": True},
        ),
        (
            "test_subtraction.vcl",
            {},
            {"subtraction": 4},
        ),
        (
            "test_tensor.vcl",
            {},
            {"tensor": (5, 2, 16, 7)},
        ),
        (
            "test_variable.vcl",
            {},
            {"variable": 2},
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
    actual_declarations = to_python(
        specification_path, target=Target.LOSS_DL2, samplers=samplers
    )
    if isinstance(validate_output, dict):
        for key in validate_output.keys():
            if validate_output[key] is not ...:
                assert validate_output[key] == actual_declarations.get(key, None)
            else:
                assert key in actual_declarations
    elif callable(validate_output):
        validate_output(actual_declarations)
