"""TensorFlow temporal builtin gating tests."""

import pytest

tf = pytest.importorskip(
    "tensorflow",
    reason="TensorFlow extra is required for TensorFlow backend tests",
)

from vehicle_lang.error import VehicleInternalError
from vehicle_lang.loss._tensorflow._builtins import TensorFlowBuiltins


def test_tensorflow_temporal_builtins_are_explicitly_unsupported() -> None:
    """Milestone-1 behavior: temporal operators should fail with deterministic diagnostics."""
    builtins = TensorFlowBuiltins()
    x = tf.constant([1.0, -1.0, 0.5], dtype=tf.float32)
    y = tf.constant([-0.2, 0.7, -0.1], dtype=tf.float32)

    with pytest.raises(VehicleInternalError, match="Globally"):
        builtins.Globally(0, 1, x)

    with pytest.raises(VehicleInternalError, match="Finally"):
        builtins.Finally(0, 2, x)

    with pytest.raises(VehicleInternalError, match="Until"):
        builtins.Until(0, 2, x, y)
