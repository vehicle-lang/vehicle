from pathlib import Path
from typing import Any, Callable, cast

import pytest
import tensorflow as tf
import vehicle_lang as vcl
import vehicle_lang.compile.tensorflow as vcl2tf


def network_validate_output(output: dict[str, Any]) -> None:
    def network(xs: Any) -> Any:
        return (sum(xs),)

    assert "prop" in output
    assert output["prop"](network) == 0.0


class DummySampler(vcl2tf.vcl.ABCSampler):
    def get_loss(
        self,
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
        minimise: bool,
    ) -> tf.Tensor:
        """Sample at a few test points in the bounded range."""
        # Sample at some test points
        test_points = [
            tf.constant(-10.0),
            tf.constant(-1.0),
            tf.constant(1.0),
            tf.constant(10.0),
        ]
        # Evaluate the search lambda at each test point
        results = []
        for point in test_points:
            result = search_lambda(cast(tf.Tensor, point))
            results.append(tf.convert_to_tensor(result))
        return tf.stack(results)


dummy_sampler = DummySampler()


@pytest.mark.parametrize(  # type: ignore[misc]
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
            {"x": dummy_sampler.get_loss},
            {"prop": 11.0},
        ),
        (
            "test_quantifier_any.vcl",
            {"x": dummy_sampler.get_loss},
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
            {"prop": 0.0},
        ),
        (
            "test_variable.vcl",
            {},
            {"prop": 0.0},
        ),
    ],
)
def test_loss_function_exec(
    specification_filename: str,
    samplers: dict[str, Any] = {},
    validate_output: dict[str, Any] | Callable[[dict[str, Any]], None] = {},
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = vcl2tf.load(
        specification_path,
        target=vcl.DifferentiableLogic.DL2,
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
