from pathlib import Path
from typing import Any, Callable, Dict, Iterator, Union

import pytest

import vehicle_lang.compile.python as vcl2py


def network_validate_output(output: Dict[str, Any]) -> None:
    network = lambda xs: (sum(xs),)
    assert "prop" in output
    assert output["prop"](network) == True


def quantifier_all_sampler(context: Dict[str, Any]) -> Iterator[Any]:
    yield from [-10.0, -1.0, 1.0, 10.0]


def quantifier_any_sampler(context: Dict[str, Any]) -> Iterator[Any]:
    yield from [-10.0, -1.0, 1.0, 10.0]


@pytest.mark.parametrize(
    "specification_filename,validate_output",
    [
        (
            "test_addition.vcl",
            {"prop": True},
        ),
        (
            "test_at.vcl",
            {"prop": False},
        ),
        (
            "test_constant.vcl",
            {"prop": True},
        ),
        (
            "test_division.vcl",
            {"prop": True},
        ),
        (
            "test_indicator.vcl",
            {"prop": False},
        ),
        (
            "test_maximum.vcl",
            {"prop": False},
        ),
        (
            "test_minimum.vcl",
            {"prop": True},
        ),
        (
            "test_multiplication.vcl",
            {"prop": True},
        ),
        (
            "test_negation.vcl",
            {"prop": True},
        ),
        (
            "test_network.vcl",
            network_validate_output,
        ),
        # (
        #     "test_quantifier_all.vcl",
        #     {"prop": True},
        # ),
        # (
        #     "test_quantifier_any.vcl",
        #     {"prop": True},
        # ),
        (
            "test_subtraction.vcl",
            {"prop": True},
        ),
        (
            "test_tensor.vcl",
            {"prop": True},
        ),
        (
            "test_variable.vcl",
            {"prop": True},
        ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_filename: str,
    validate_output: Union[Dict[str, Any], Callable[[Dict[str, Any]], None]],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    output = vcl2py.load(specification_path)
    if isinstance(validate_output, dict):
        for key in validate_output.keys():
            if validate_output[key] is not ...:
                assert validate_output[key] == output.get(key, None)
            else:
                assert key in output
    elif callable(validate_output):
        validate_output(output)


test_loss_function_exec("test_maximum.vcl", {"prop": False})
