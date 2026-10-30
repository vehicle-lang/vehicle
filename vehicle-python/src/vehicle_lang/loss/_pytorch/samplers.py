from abc import abstractmethod
from dataclasses import dataclass
from typing import TYPE_CHECKING, Callable, List, Sequence

from jaxtyping import Float

from ..._deps import require_optional_dependency
from .._abc import ABCSampler
from .._common import BoundVarData

if TYPE_CHECKING:
    import torch
else:  # pragma: no cover - exercised implicitly
    torch = require_optional_dependency(
        "torch",
        extra="pytorch",
        feature="The PyTorch loss backend",
    )


class PyTorchSampler(ABCSampler[Sequence[int], torch.Tensor]):
    def get_loss_and_input(
        self,
        dims: Sequence[int],
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
    ) -> tuple[Float[torch.Tensor, "1 losses"], torch.Tensor]:
        """Validates the sampling domain and calls the core sampler implementation."""
        self._validate_domain(lower_bound, upper_bound)
        return self._get_loss_and_input(dims, lower_bound, upper_bound, search_lambda)

    def _validate_domain(
        self,
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
    ) -> None:
        """Checks that the sampling domain is non-empty."""
        if not torch.all(lower_bound <= upper_bound).item():
            raise ValueError("Empty sampling domain: lower bound exceeds upper bound.")

    @abstractmethod
    def _get_loss_and_input(
        self,
        dims: Sequence[int],
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
    ) -> tuple[Float[torch.Tensor, "1 losses"], torch.Tensor]:
        """
        Calls the core sampling procedure for the specific backend.
        Uses gradient ascent or descent to generate samples and evaluate the search lambda.
        """
        ...

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
    Default sampler implementation for PyTorch that uses Projected Gradient Descent attack
    to generate adversarial samples, descending the search_lambda so that the samples approximate
    its infimum.
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

    def _get_loss_and_input(
        self,
        dims: Sequence[int],
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
    ) -> tuple[Float[torch.Tensor, "1 losses"], torch.Tensor]:
        """
        Uses gradient ascent or descent to generate samples and evaluate the search lambda.

        The step size is automatically inferred from the bounds to provide
        an out-of-the-box implementation that works for most applications.

        Args:
            dims: The dimensions for the sampling
            lower_bound: The lower bound tensor
            upper_bound: The upper bound tensor
            search_lambda: A callable representing a loss function

        Returns:
            A sequence of loss values evaluated at the PGD-perturbed points and a sequence
            of those points.
        """
        # Set seed for reproducibility if provided
        if self.seed is not None:
            torch.manual_seed(self.seed)

        # Infer step size from bounds: use a fraction of the range
        start_low, start_high = self.starting_region(
            lower_bound, upper_bound, self.unbounded_search_distance
        )
        range_size = start_high - start_low
        epsilon = range_size / self.num_steps

        loss_values = []
        points = []

        # Use multiple random starting points to ensure diversity
        for _ in range(self.num_samples):
            # Start from a random initial point in the valid range
            current_point = (
                start_low + torch.rand(dims, dtype=lower_bound.dtype) * range_size
            )

            # Perform PGD iterations from this starting point
            # IMPORTANT: During PGD, we only want gradients w.r.t. the INPUT to find
            # adversarial examples. We must NOT accumulate gradients in network parameters,
            # as that would interfere with the actual training gradients computed later.
            for _ in range(self.num_steps):
                # Enable gradient computation for the current point
                current_point_var = current_point.detach().clone().requires_grad_(True)

                # Enable gradient tracking so that we can compute the gradients
                # for the search even if the loss function is called inside torch.no_grad().
                with torch.enable_grad():
                    # Compute gradient of search_lambda with respect to input
                    loss = search_lambda(current_point_var)

                    # Compute gradient ONLY w.r.t. the input, not network weights
                    # Using autograd.grad instead of backward() to avoid accumulating
                    # gradients in network parameters during adversarial search
                    gradient = torch.autograd.grad(
                        loss,
                        current_point_var,
                        create_graph=False,  # Don't need second-order gradients
                        retain_graph=False,  # Don't need to backprop again
                        only_inputs=True,  # Only compute for inputs, not all parameters
                    )[0]

                    # If gradient contains NaN, replace with zeros
                    if gradient is not None:
                        gradient = torch.where(
                            torch.isnan(gradient), torch.zeros_like(gradient), gradient
                        )
                    else:
                        gradient = torch.zeros_like(current_point_var)

                # We are searching for the infimum of the lambda,
                # so we need to follow the gradient downards to find the most true value.
                perturbation = -epsilon * torch.sign(gradient)

                # Apply perturbation and clip to bounds
                current_point = torch.clamp(
                    current_point + perturbation.detach(), lower_bound, upper_bound
                )

            points.append(current_point)
            # Evaluate and store the final result from this trajectory
            loss = search_lambda(current_point.detach())
            loss_values.append(torch.as_tensor(loss))

        return torch.stack(loss_values), torch.stack(points)
