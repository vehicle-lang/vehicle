import pytest

tf = pytest.importorskip(
    "tensorflow",
    reason="TensorFlow extra is required for loss execution tests",
)

from typing import Any, Callable

import vehicle_lang as vcl
from vehicle_lang.loss import tensorflow as loss_tf
from vehicle_lang.loss._tensorflow.samplers import (
    ConstantTensorFlowSampler,
    TensorFlowSampler,
)

from ..config import PYTHON_TEST_SPECS_PATH

###########
## Setup ##
###########


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


###################
## Execute tests ##
###################


@pytest.mark.parametrize(  # type: ignore[untyped-decorator]
    "specification_filename,samplers,validate_output",
    [
        # TODO: re-enable when we have pure exports
        # (
        #     "test_addition.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_at.vcl",
        #     None,
        #     {"prop": -1.0},
        # ),
        # (
        #     "test_constant.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_division.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_indicator.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_maximum.vcl",
        #     None,
        #     {"prop": 3.5},
        # ),
        # (
        #     "test_minimum.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_multiplication.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_negation.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        (
            "test_network.vcl",
            None,
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
            {"x": ConstantTensorFlowSampler(tf.constant([0.0]))},
            validate_loss_function_output,
        ),
        # (
        #     "test_subtraction.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_tensor.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
        # (
        #     "test_variable.vcl",
        #     None,
        #     {"prop": 0.0},
        # ),
    ],
)
def test_loss_function_exec(
    specification_filename: str,
    samplers: dict[str, TensorFlowSampler],
    validate_output: dict[str, Any] | Callable[[dict[str, Any]], None],
) -> None:
    print(f"Exec {specification_filename}")
    specification_path = PYTHON_TEST_SPECS_PATH / specification_filename
    actual_declarations = loss_tf.load_specification(
        specification_path,
        logic=vcl.DL2DifferentiableLogic(),
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
