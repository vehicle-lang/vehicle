from functools import reduce
from pathlib import Path
from typing import Any, Callable, Dict, Union

import numpy as np
import pytest
import vehicle_lang as vcl
import vehicle_lang.compile.python as vcl2py
import vehicle_lang.typing as vclt


def network_validate_output(output: Dict[str, Any]) -> None:
    network = lambda xs: (sum(xs),)
    assert "prop" in output
    assert output["prop"](network) == 0.0


domain_0to1 = vcl.PythonVariableDomain.from_bounds(0, 1, dtype=np.float32)


def quantifier_all_optimiser(
    variable: str,
    _domain: vcl.PythonVariableDomain[np.float32],
    _minimise: bool,
    _context: Dict[str, Any],
    joiner: Callable[[np.float32, np.float32], np.float32],
    predicate: Callable[[np.float32], np.float32],
) -> np.float32:
    return reduce(joiner, [predicate(np.float32(v)) for v in [-10.0, -1.0, 1.0, 10.0]])


def quantifier_any_optimiser(
    variable: str,
    _domain: vcl.PythonVariableDomain[np.float32],
    _minimise: bool,
    _context: Dict[str, Any],
    joiner: Callable[[np.float32, np.float32], np.float32],
    predicate: Callable[[np.float32], np.float32],
) -> np.float32:
    return reduce(joiner, [predicate(np.float32(v)) for v in [-10.0, -1.0, 1.0, 10.0]])


@pytest.mark.parametrize(
    "specification_filename,quantified_variable_domains,quantified_variable_optimisers,validate_output",
    [
        (
            "test_addition.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_at.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_constant.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_division.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_indicator.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_maximum.vcl",
            {},
            {},
            {"prop": 3.5},
        ),
        (
            "test_minimum.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_multiplication.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_negation.vcl",
            {},
            {},
            {"prop": 0.0},
        ),
        (
            "test_network.vcl",
            {},
            {},
            network_validate_output,
        ),
        (
            "test_quantifier_all.vcl",
            {"x": lambda _ctx: domain_0to1},
            {"x": quantifier_all_optimiser},
            {"prop": 11.0},
        ),
        (
            "test_quantifier_any.vcl",
            {"x": lambda _ctx: domain_0to1},
            {"x": quantifier_any_optimiser},
            {"prop": 1.0},
        ),
        (
            "test_subtraction.vcl",
            {},
            {},
            {"prop": 1.0},
        ),
        (
            "test_tensor.vcl",
            {},
            {},
            {"prop": 1.0},
        ),
        (
            "test_variable.vcl",
            {},
            {},
            {"prop": 1.0},
        ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    specification_filename: str,
    quantified_variable_domains: vclt.Domains[
        Any, Union[np.uint64, np.int64, np.float32]
    ],
    quantified_variable_optimisers: vclt.Optimisers[
        vclt.QuantifiedVariableName,
        Any,
        Union[np.uint64, np.int64, np.float32],
        np.float32,
    ],
    validate_output: Union[Dict[str, Any], Callable[[Dict[str, Any]], None]],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = vcl2py.load(
        path=specification_path,
        quantified_variable_domains=quantified_variable_domains,
        quantified_variable_optimisers=quantified_variable_optimisers,
        target=vcl.DifferentiableLogic.DL2,
    )
    if isinstance(validate_output, dict):
        for key in validate_output.keys():
            if validate_output[key] is not ...:
                assert validate_output[key] == actual_declarations.get(key, None)
            else:
                assert key in actual_declarations
    elif callable(validate_output):
        validate_output(actual_declarations)


if __name__ == "__main__":
    pytest.main(["vehicle-python/tests/test_lossdl2_exec.py"])
