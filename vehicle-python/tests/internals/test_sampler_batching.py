"""The default sampler batches its trajectories, and must match the sequential search exactly.

`DefaultPyTorchSampler` lifts the compiled single-point search over a batch axis with `vmap`,
so every step is one forward and backward pass over all starting points. A sequential
reference with the same seed, starting points and step rule has to agree to float precision.
"""

from typing import Any, Callable, Sequence

import pytest

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)

from vehicle_lang.loss._pytorch.samplers import DefaultPyTorchSampler


def _sequential(
    sampler: DefaultPyTorchSampler,
    dims: Sequence[int],
    lower: torch.Tensor,
    upper: torch.Tensor,
    search: Callable[[torch.Tensor], torch.Tensor],
    quantifier: str,
) -> torch.Tensor:
    """One trajectory at a time, as the sampler did before it was batched."""
    torch.manual_seed(sampler.seed)
    start_low, start_high = sampler.starting_region(
        lower, upper, sampler.unbounded_search_distance
    )
    range_size = start_high - start_low
    epsilon = range_size / sampler.num_steps
    step = epsilon if quantifier == "Forall" else -epsilon
    starts = (
        start_low
        + torch.rand((sampler.num_samples, *dims), dtype=lower.dtype) * range_size
    )
    results = []
    for point in starts:
        for _ in range(sampler.num_steps):
            var = point.detach().clone().requires_grad_(True)
            gradient = torch.autograd.grad(search(var).reshape(()), var)[0]
            gradient = torch.where(
                torch.isnan(gradient), torch.zeros_like(gradient), gradient
            )
            point = torch.clamp(point + step * torch.sign(gradient), lower, upper)
        results.append(search(point.detach()).reshape(()))
    return torch.stack(results)


@pytest.mark.parametrize("quantifier", ["Forall", "Exists"])
def test_batched_search_matches_sequential(quantifier: str) -> None:
    # A bumpy surface, so trajectories from different starts end in different places.
    def search(x: torch.Tensor) -> torch.Tensor:
        return torch.sin(3.0 * x[0]) * torch.cos(2.0 * x[1]) + 0.1 * (x[0] - x[1]) ** 2

    lower, upper = torch.tensor([-2.0, -2.0]), torch.tensor([2.0, 2.0])
    sampler = DefaultPyTorchSampler(num_samples=16, num_steps=6, seed=3)

    batched = sampler.get_loss([2], lower, upper, search, quantifier)
    reference = _sequential(sampler, [2], lower, upper, search, quantifier)

    assert batched.shape == (16,)
    torch.testing.assert_close(batched, reference, rtol=0, atol=1e-6)


@pytest.mark.skipif(not torch.cuda.is_available(), reason="needs a CUDA device")
def test_batched_search_stays_on_the_bounds_device() -> None:
    device = torch.device("cuda")
    lower, upper = torch.zeros(3, device=device), torch.ones(3, device=device)
    sampler = DefaultPyTorchSampler(num_samples=8, num_steps=2, seed=0)
    losses = sampler.get_loss([3], lower, upper, lambda x: (x * x).sum(), "Forall")
    assert losses.device.type == "cuda"


def test_sampler_keeps_its_final_points() -> None:
    lower, upper = torch.tensor([-1.0, -1.0]), torch.tensor([1.0, 1.0])
    sampler = DefaultPyTorchSampler(num_samples=5, num_steps=3, seed=1)
    losses = sampler.get_loss([2], lower, upper, lambda x: (x * x).sum(), "Forall")
    assert sampler.last_points is not None and sampler.last_points.shape == (5, 2)
    torch.testing.assert_close((sampler.last_points**2).sum(dim=1), losses)
