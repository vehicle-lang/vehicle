"""A default sampler must search in its quantifier's direction.

An existential approximates an infimum of the lambda and so descends it; a
universal approximates a supremum and so ascends. With `num_steps` steps of
`range / num_steps` from any starting point in the bounds, a monotone lambda is
driven all the way to one end, which makes the expected value exact rather than
approximate.
"""

import pytest

BOUNDS = (0.0, 1.0)
STEPS = 5


@pytest.mark.parametrize(
    ("quantifier", "expected"), [("Exists", BOUNDS[0]), ("Forall", BOUNDS[1])]
)  # type: ignore[untyped-decorator]
def test_pytorch_sampler_searches_in_the_quantifiers_direction(
    quantifier: str, expected: float
) -> None:
    torch = pytest.importorskip("torch", reason="PyTorch extra is required")
    from vehicle_lang.loss._pytorch.samplers import DefaultPyTorchSampler

    sampler = DefaultPyTorchSampler(num_samples=3, num_steps=STEPS, seed=0)
    losses = sampler.get_loss(
        (),
        torch.tensor(BOUNDS[0]),
        torch.tensor(BOUNDS[1]),
        lambda x: x,
        quantifier,
    )
    assert torch.allclose(losses, torch.full_like(losses, expected))


@pytest.mark.parametrize(
    ("quantifier", "expected"), [("Exists", BOUNDS[0]), ("Forall", BOUNDS[1])]
)  # type: ignore[untyped-decorator]
def test_tensorflow_sampler_searches_in_the_quantifiers_direction(
    quantifier: str, expected: float
) -> None:
    tf = pytest.importorskip("tensorflow", reason="TensorFlow extra is required")
    from vehicle_lang.loss._tensorflow.samplers import DefaultTensorFlowSampler

    sampler = DefaultTensorFlowSampler(num_samples=3, num_steps=STEPS, seed=0)
    losses = sampler.get_loss(
        (),
        tf.constant(BOUNDS[0]),
        tf.constant(BOUNDS[1]),
        lambda x: x,
        quantifier,
    )
    assert tf.reduce_all(tf.abs(losses - expected) < 1e-6)
