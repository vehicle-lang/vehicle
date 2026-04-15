from pathlib import Path
from typing import Any, Callable, Sequence

import pytest

import vehicle_lang as vcl
from vehicle_lang.error import VehicleInternalError

tf = pytest.importorskip(
    "tensorflow",
    reason="TensorFlow extra is required for loss execution tests",
)
from vehicle_lang.loss import tensorflow as loss_tf


def network_validate_output(output: dict[str, Any]) -> None:
    def network(xs: Any) -> Any:
        return (sum(xs),)

    assert "prop" in output
    assert output["prop"](network) == 0.0


def validate_loss_function_output(
    output: dict[str, Any],
) -> None:
    """Validate the loss function by checking its value at specific points."""
    test_points = [-5.0, 0.0, 0.5, 1.0, 5.0]

    # The bounded function takes a network as a positional argument
    func = output["bounded"]
    for point in test_points:

        def test_network(x: Any) -> Any:
            return tf.constant([point])

        loss_value = func(test_network)
        assert isinstance(loss_value, tf.Tensor)
        assert loss_value.shape == ()


def validate_temporal_tensorflow_gating(output: dict[str, Any]) -> None:
    """Temporal operators should fail explicitly on TensorFlow in milestone 1."""

    def test_network(_x: Any) -> Any:
        return tf.constant([1.0, -1.0, 0.5, -0.2], dtype=tf.float32)

    for prop_name, op_name in [
        ("prop_globally", "Globally"),
        ("prop_finally", "Finally"),
        ("prop_until", "Until"),
    ]:
        assert prop_name in output
        with pytest.raises(VehicleInternalError, match=op_name):
            output[prop_name](test_network)


class DummySampler(loss_tf.TensorFlowSampler):  # type: ignore[misc]
    def get_loss(
        self,
        dims: Sequence[int],
        lower_bound: Any,
        upper_bound: Any,
        search_lambda: Callable[[Any], Any],
        minimise: bool,
    ) -> Any:
        """Sample at a few test points in the bounded range."""
        # Sample at some test points
        test_points = [
            tf.constant([-10.0]),
            tf.constant([-1.0]),
            tf.constant([1.0]),
            tf.constant([10.0]),
        ]
        # Evaluate the search lambda at each test point
        results = []
        for point in test_points:
            result = search_lambda(point)
            results.append(tf.convert_to_tensor(result))
        return tf.stack(results)


dummy_sampler = DummySampler()


@pytest.mark.parametrize(  # type: ignore[untyped-decorator]
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
            {"prop": 1000000.0},
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
            {"prop": 1000000.0},
        ),
        (
            "test_maximum.vcl",
            {},
            {"prop": 1000000.0},
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
        # (
        #     "test_quantifier_all.vcl",
        #     {"x": dummy_sampler.get_loss},
        #     {"prop": 11.0},
        # ),
        # (
        #     "test_quantifier_any.vcl",
        #     {"x": dummy_sampler.get_loss},
        #     {"prop": 0.0},
        # ),
        (
            "test_bounded.vcl",
            {"x": dummy_sampler.get_loss},
            validate_loss_function_output,
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
        (
            "test_temporal_runtime.vcl",
            {},
            validate_temporal_tensorflow_gating,
        ),
    ],
)
def test_loss_function_exec(
    specification_filename: str,
    samplers: dict[str, Any],
    validate_output: dict[str, Any] | Callable[[dict[str, Any]], None],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = Path(__file__).parent / "data" / specification_filename
    actual_declarations = loss_tf.load_specification(
        specification_path,
        logic=vcl.DifferentiableLogic.DL2,
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
