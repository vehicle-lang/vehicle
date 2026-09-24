from abc import abstractmethod
from typing import TYPE_CHECKING, Callable, Sequence

from jaxtyping import Float

from ..._ast._nodes import Quantifier
from ..._deps import require_optional_dependency
from .._abc import ABCSampler

if TYPE_CHECKING:
    import torch
else:  # pragma: no cover - exercised implicitly
    torch = require_optional_dependency(
        "torch",
        extra="pytorch",
        feature="The PyTorch loss backend",
    )


class PyTorchSampler(ABCSampler[Sequence[int], torch.Tensor]):
    @abstractmethod
    def get_loss(
        self,
        dims: Sequence[int],
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
        quantifier: Quantifier,
    ) -> Float[torch.Tensor, "1 losses"]: ...

    @staticmethod
    def starting_region(
        lower_bound: torch.Tensor, upper_bound: torch.Tensor, distance: float
    ) -> tuple[torch.Tensor, torch.Tensor]:
        """
        Give each unbounded endpoint, which the domain represents as -infinity or infinity, a
        finite one `distance` away, leaving bounded endpoints alone.
        """
        low_infinite = torch.isneginf(lower_bound)
        high_infinite = torch.isposinf(upper_bound)
        origin = torch.zeros_like(lower_bound)
        low_anchor = torch.where(high_infinite, origin, upper_bound)
        high_anchor = torch.where(low_infinite, origin, lower_bound)
        start_low = torch.where(low_infinite, low_anchor - distance, lower_bound)
        start_high = torch.where(high_infinite, high_anchor + distance, upper_bound)
        return start_low, start_high


class DefaultPyTorchSampler(PyTorchSampler):
    """
    Default sampler implementation for PyTorch that uses FGSM attack.

    After a call, `last_points` holds the final point of every trajectory, in the units of the
    quantified variable.

    Uses Fast Gradient Sign Method (FGSM) to generate adversarial samples, descending the
    search_lambda for an existential and ascending it for a universal.
    """

    def __init__(
        self,
        num_samples: int = 10,
        num_steps: int = 5,
        seed: int | None = None,
        unbounded_search_distance: float = 10.0,
    ):
        """
        Initialize the FGSM sampler.

        Args:
            num_samples: Number of independent random starting points (default: 10)
            num_steps: Number of FGSM iterations per starting point (default: 5)
            seed: Random seed for reproducibility (default: None)
            unbounded_search_distance: How far to search along a dimension the domain leaves
                unbounded: from the opposite bound, or from the origin when the dimension
                is unbounded in both directions (default: 10.0)
        """
        self.num_samples = num_samples
        self.num_steps = num_steps
        self.seed = seed
        self.unbounded_search_distance = unbounded_search_distance
        self.last_points: torch.Tensor | None = None

    def get_loss(
        self,
        dims: Sequence[int],
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
        quantifier: Quantifier,
    ) -> Float[torch.Tensor, "1 losses"]:
        """
        Use PGD to generate adversarial samples and evaluate the search lambda.

        The step size is automatically inferred from the bounds to provide
        an out-of-the-box implementation that works for most applications.

        Args:
            dims: The dimensions for the sampling (currently unused for scalar sampling)
            lower_bound: The lower bound tensor
            upper_bound: The upper bound tensor
            search_lambda: A callable representing the property to evaluate

        Returns:
            A sequence of loss values evaluated at the PGD-perturbed points
        """
        # Set seed for reproducibility if provided
        if self.seed is not None:
            torch.manual_seed(self.seed)

        device, dtype = lower_bound.device, lower_bound.dtype

        # Infer step size from bounds: use a fraction of the range
        start_low, start_high = self.starting_region(
            lower_bound, upper_bound, self.unbounded_search_distance
        )
        range_size = start_high - start_low
        epsilon = range_size / self.num_steps

        # The compiled search lambda is written for one point. `vmap` lifts it over a leading
        # batch axis, so every trajectory takes each step in a single forward and backward pass
        # rather than one pass per point.
        def scalar_search(point: torch.Tensor) -> torch.Tensor:
            return search_lambda(point).reshape(())

        batched_gradient = torch.func.vmap(torch.func.grad(scalar_search))
        batched_search = torch.func.vmap(scalar_search)

        # FGSM: an existential wants the infimum of the lambda so descends it, a universal
        # wants the supremum so ascends. For a universal the lambda is the property itself,
        # so ascending is what hunts the worst case.
        step = epsilon if quantifier == "Forall" else -epsilon

        # Start every trajectory from its own random point in the valid range.
        points = (
            start_low
            + torch.rand((self.num_samples, *dims), dtype=dtype, device=device)
            * range_size
        )

        # Gradient tracking is enabled explicitly so the search works when the loss function
        # is called inside torch.no_grad(). Only the input is differentiated, so nothing
        # accumulates in the network's parameters.
        with torch.enable_grad():
            for _ in range(self.num_steps):
                gradient = batched_gradient(points.detach())
                gradient = torch.where(
                    torch.isnan(gradient), torch.zeros_like(gradient), gradient
                )
                points = torch.clamp(
                    points + step * torch.sign(gradient), lower_bound, upper_bound
                )

        # Evaluate every trajectory's final point. The points are kept so a caller can measure
        # something other than the lambda's value at them, such as whether the property holds.
        self.last_points = points.detach()
        return batched_search(points.detach())
