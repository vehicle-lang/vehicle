from functools import reduce
from pathlib import Path
from typing import Any, Callable, Dict, Union

import pytest

import vehicle_lang as vcl
import vehicle_lang.compile.python as vcl2py


def network_validate_output(output: Dict[str, Any]) -> None:
    network = lambda xs: (sum(xs),)
    assert "prop" in output
    assert output["prop"](network) == 0.0


def quantifier_all_optimiser(
    _minimise: bool,
    _context: Dict[str, Any],
    joiner: Callable[[float, float], float],
    predicate: Callable[[Any], float],
) -> float:
    return reduce(joiner, [predicate(v) for v in [-10.0, -1.0, 1.0, 10.0]])


def quantifier_any_optimiser(
    _minimise: bool,
    _context: Dict[str, Any],
    joiner: Callable[[float, float], float],
    predicate: Callable[[Any], float],
) -> float:
    return reduce(joiner, [predicate(v) for v in [-10.0, -1.0, 1.0, 10.0]])


@pytest.mark.parametrize(
    "specification_filename,optimisers,validate_output",
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
            {"prop": True},
        ),
        (
            "test_division.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_indicator.vcl",
            {},
            {"prop": False},
        ),
        (
            "test_maximum.vcl",
            {},
            {"prop": False},
        ),
        (
            "test_minimum.vcl",
            {},
            {"prop": True},
        ),
        (
            "test_multiplication.vcl",
            {},
            {"prop": 0.0},
        ),
        (
            "test_negation.vcl",
            {},
            {"prop": True},
        ),
        (
            "test_network.vcl",
            {},
            network_validate_output,
        ),
        (
            "test_quantifier_all.vcl",
            {"x": quantifier_all_optimiser},
            {"prop": 11.0},
        ),
        (
            "test_quantifier_any.vcl",
            {"x": quantifier_any_optimiser},
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
            {"prop": True},
        ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_filename: str,
    optimisers: Dict[str, Any],
    validate_output: Union[Dict[str, Any], Callable[[Dict[str, Any]], None]],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = vcl2py.load(
        specification_path,
        target=vcl.DifferentiableLogic.DL2,
        translation=vcl2py.PythonTranslation.from_optimisers(optimisers),
    )
    if isinstance(validate_output, dict):
        for key in validate_output.keys():
            if validate_output[key] is not ...:
                assert validate_output[key] == actual_declarations.get(key, None)
            else:
                assert key in actual_declarations
    elif callable(validate_output):
        validate_output(actual_declarations)
