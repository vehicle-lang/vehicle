"""TensorFlow temporal builtin gating tests."""

import pytest

tf = pytest.importorskip(
    "tensorflow",
    reason="TensorFlow extra is required for TensorFlow backend tests",
)

from vehicle_lang.error import VehicleInternalError
from vehicle_lang.loss._tensorflow._builtins import TensorFlowBuiltins


def test_tensorflow_temporal_builtins_are_unsupported() -> None:
    """Temporal operators are unsupported in the TensorFlow backend.

    Temporal operators (Globally, Finally, Until) require vehicle-stl, which
    only provides a PyTorch implementation.  The TensorFlow backend raises
    ``VehicleInternalError`` immediately with a descriptive message so that
    users get a clear diagnostic rather than a silent wrong result.
    """
    builtins = TensorFlowBuiltins()
    x = tf.constant([1.0, -1.0, 0.5], dtype=tf.float32)
    y = tf.constant([-0.2, 0.7, -0.1], dtype=tf.float32)

    with pytest.raises(VehicleInternalError, match="Globally"):
        builtins.Globally(0, 1, x)

    with pytest.raises(VehicleInternalError, match="Finally"):
        builtins.Finally(0, 2, x)

    with pytest.raises(VehicleInternalError, match="Until"):
        builtins.Until(0, 2, x, y)
