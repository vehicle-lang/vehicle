"""A default sampler must descend the lambda it is given.

The search approximates an infimum of the lambda, so both backends have to move
against the gradient. With `num_steps` steps of `range / num_steps` from any
starting point in the bounds, a monotone lambda is driven to the low end, which
makes the expected value exact rather than approximate.
"""

import pytest

BOUNDS = (0.0, 1.0)
STEPS = 5


def test_pytorch_sampler_descends_the_lambda() -> None:
    torch = pytest.importorskip("torch", reason="PyTorch extra is required")
    from vehicle_lang.loss._pytorch.samplers import DefaultPyTorchSampler

    sampler = DefaultPyTorchSampler(num_samples=3, num_steps=STEPS, seed=0)
    losses = sampler.get_loss(
        (),
        torch.tensor(BOUNDS[0]),
        torch.tensor(BOUNDS[1]),
        lambda x: x,
    )
    assert torch.allclose(losses, torch.zeros_like(losses))


def test_tensorflow_sampler_descends_the_lambda() -> None:
    tf = pytest.importorskip("tensorflow", reason="TensorFlow extra is required")
    from vehicle_lang.loss._tensorflow.samplers import DefaultTensorFlowSampler

    sampler = DefaultTensorFlowSampler(num_samples=3, num_steps=STEPS, seed=0)
    losses = sampler.get_loss(
        (),
        tf.constant(BOUNDS[0]),
        tf.constant(BOUNDS[1]),
        lambda x: x,
    )
    assert tf.reduce_all(tf.abs(losses) < 1e-6)
