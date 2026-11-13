"""Test rejection of empty sampling domains (lower > upper)"""

from typing import Any, Tuple

import pytest
import vehicle_lang as vcl

from ..config import PYTHON_TEST_SPECS_PATH

SPEC_PATH = PYTHON_TEST_SPECS_PATH / "test_empty_sampling_domain.vcl"


def require_tensorflow() -> Tuple[Any, Any]:
    tf_module = pytest.importorskip(
        "tensorflow",
        reason="TensorFlow extra is required for TensorFlow training tests",
    )
    loss_module = pytest.importorskip(
        "vehicle_lang.loss.tensorflow",
        reason="vehicle_lang[tensorflow] extra is not installed",
    )
    return tf_module, loss_module


def require_pytorch() -> Tuple[Any, Any]:
    torch_module = pytest.importorskip(
        "torch",
        reason="PyTorch extra is required for PyTorch training tests",
    )
    loss_module = pytest.importorskip(
        "vehicle_lang.loss.pytorch",
        reason="vehicle_lang[pytorch] extra is not installed",
    )
    return torch_module, loss_module


def test_empty_sampling_domain_rejection_pytorch() -> None:
    """Test that PyTorch implementation rejects an empty domain before invoking _get_loss_and_input()."""
    torch, loss_pt = require_pytorch()
    from vehicle_lang.loss.pytorch import PyTorchSampler

    class FailingPyTorchSampler(PyTorchSampler):
        def _get_loss_and_input(
            self,
            dims: Any,
            lower_bound: Any,
            upper_bound: Any,
            search_lambda: Any,
        ) -> Any:
            raise AssertionError(
                "_get_loss_and_input() should not be called with an empty domain."
            )

    declarations = loss_pt.load_specification(
        SPEC_PATH,
        logic=vcl.DL2DifferentiableLogic(),
        samplers={"x": FailingPyTorchSampler()},
    )

    property_loss = declarations["p"]

    # A simple identity network is sufficient while testing domain validation
    def network(x: Any) -> Any:
        return x.reshape(1)

    # An empty sampling domain (1, -1) should be rejected
    # before FailingPyTorchSampler._get_loss_and_input() is called
    with pytest.raises(
        ValueError,
        match="Empty sampling domain",
    ):
        property_loss(
            network,
            torch.tensor(1.0),
            torch.tensor(-1.0),
        )


def test_empty_sampling_domain_rejection_tensorflow() -> None:
    """Test that TensorFlow implementation rejects an empty domain before invoking _get_loss_and_input()."""
    tf, loss_tf = require_tensorflow()
    from vehicle_lang.loss.tensorflow import TensorFlowSampler

    class FailingTensorFlowSampler(TensorFlowSampler):
        def _get_loss_and_input(
            self,
            dims: Any,
            lower_bound: Any,
            upper_bound: Any,
            search_lambda: Any,
        ) -> Any:
            raise AssertionError(
                "_get_loss_and_input() should not be called with an empty domain."
            )

    declarations = loss_tf.load_specification(
        SPEC_PATH,
        logic=vcl.DL2DifferentiableLogic(),
        samplers={"x": FailingTensorFlowSampler()},
    )

    property_loss = declarations["p"]

    # A simple identity network is sufficient while testing domain validation
    def network(x: Any) -> Any:
        return tf.reshape(x, [1])

    # An empty sampling domain (1, -1) should be rejected
    # before FailingTensorFlowSampler._get_loss_and_input() is called
    with pytest.raises(
        tf.errors.InvalidArgumentError,
        match="Empty sampling domain",
    ):
        property_loss(
            network,
            tf.constant(1.0),
            tf.constant(-1.0),
        )
