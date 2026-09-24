"""Test that the default samplers cope with a variable the domain leaves unbounded."""

from typing import Any, Tuple

import pytest


def require_pytorch() -> Tuple[Any, Any]:
    torch_module = pytest.importorskip(
        "torch",
        reason="PyTorch extra is required for PyTorch sampler tests",
    )
    samplers_module = pytest.importorskip(
        "vehicle_lang.loss._pytorch.samplers",
        reason="vehicle_lang[pytorch] extra is not installed",
    )
    return torch_module, samplers_module


def require_tensorflow() -> Tuple[Any, Any]:
    tf_module = pytest.importorskip(
        "tensorflow",
        reason="TensorFlow extra is required for TensorFlow sampler tests",
    )
    samplers_module = pytest.importorskip(
        "vehicle_lang.loss._tensorflow.samplers",
        reason="vehicle_lang[tensorflow] extra is not installed",
    )
    return tf_module, samplers_module


@pytest.mark.parametrize(  # type: ignore[untyped-decorator]
    "quantifier", ["Forall", "Exists"]
)
@pytest.mark.parametrize(  # type: ignore[untyped-decorator]
    "lower,upper",
    [
        (float("-inf"), float("inf")),
        (float("-inf"), 1.0),
        (0.0, float("inf")),
    ],
)
def test_pytorch_sampler_handles_unbounded_variables(
    lower: float, upper: float, quantifier: Any
) -> None:
    torch, samplers = require_pytorch()
    sampler = samplers.DefaultPyTorchSampler(num_samples=4, num_steps=3, seed=0)

    # The search is batched with `vmap`, so the points it sees cannot be captured and
    # inspected afterwards. The lambda returns the point itself instead, so the losses
    # are the final points and the bounds check is made on them.
    def search(x: Any) -> Any:
        return x[0]

    points = sampler.get_loss(
        [1], torch.tensor([lower]), torch.tensor([upper]), search, quantifier
    )

    assert torch.isfinite(points).all(), "sampled points must be finite"
    assert bool((points >= lower).all()) and bool((points <= upper).all())


@pytest.mark.parametrize(  # type: ignore[untyped-decorator]
    "quantifier", ["Forall", "Exists"]
)
@pytest.mark.parametrize(  # type: ignore[untyped-decorator]
    "lower,upper",
    [
        (float("-inf"), float("inf")),
        (float("-inf"), 1.0),
        (0.0, float("inf")),
    ],
)
def test_tensorflow_sampler_handles_unbounded_variables(
    lower: float, upper: float, quantifier: Any
) -> None:
    tf, samplers = require_tensorflow()
    sampler = samplers.DefaultTensorFlowSampler(num_samples=4, num_steps=3, seed=0)

    seen = []

    def search(x: Any) -> Any:
        seen.append(x)
        return x[0] * 2.0

    losses = sampler.get_loss(
        [1], tf.constant([lower]), tf.constant([upper]), search, quantifier
    )

    assert bool(
        tf.reduce_all(tf.math.is_finite(losses))
    ), "sampled losses must be finite"
    for point in seen:
        assert bool(
            tf.reduce_all(tf.math.is_finite(point))
        ), "sampled points must be finite"
        assert bool(tf.reduce_all(point >= lower)) and bool(
            tf.reduce_all(point <= upper)
        )
