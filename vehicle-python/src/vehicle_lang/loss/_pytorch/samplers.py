from abc import abstractmethod
from typing import TYPE_CHECKING, Callable, Sequence

from jaxtyping import Float

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
        minimise: bool,
    ) -> Float[torch.Tensor, "1 losses"]: ...


class DefaultPyTorchSampler(PyTorchSampler):
    """
    Default sampler implementation for PyTorch that uses FGSM attack.

    Uses Fast Gradient Sign Method (FGSM) to generate adversarial samples
    that explore the search space by perturbing points in the direction
    of the gradient to maximize or minimize the search_lambda.
    """

    def __init__(
        self, num_samples: int = 10, num_steps: int = 5, seed: int | None = None
    ):
        """
        Initialize the FGSM sampler.

        Args:
            num_samples: Number of independent random starting points (default: 10)
            num_steps: Number of FGSM iterations per starting point (default: 5)
            seed: Random seed for reproducibility (default: None)
        """
        self.num_samples = num_samples
        self.num_steps = num_steps
        self.seed = seed

    def get_loss(
        self,
        dims: Sequence[int],
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
        minimise: bool,
    ) -> Float[torch.Tensor, "1 losses"]:
        """
        Use FGSM to generate adversarial samples and evaluate the search lambda.

        The step size is automatically inferred from the bounds to provide
        an out-of-the-box implementation that works for most applications.

        Args:
            dims: The dimensions for the sampling (currently unused for scalar sampling)
            lower_bound: The lower bound tensor
            upper_bound: The upper bound tensor
            search_lambda: A callable representing the property to evaluate
            minimise: Whether to minimize (True) or maximize (False) the search_lambda

        Returns:
            A sequence of loss values evaluated at the FGSM-perturbed points
        """
        # Set seed for reproducibility if provided
        if self.seed is not None:
            torch.manual_seed(self.seed)

        # Infer step size from bounds: use a fraction of the range
        range_size = upper_bound - lower_bound
        epsilon = range_size / self.num_steps

        results = []

        # Use multiple random starting points to ensure diversity
        for _ in range(self.num_samples):
            # Start from a random initial point in the valid range
            current_point = (
                lower_bound + torch.rand((), dtype=lower_bound.dtype) * range_size
            )

            # Perform FGSM iterations from this starting point
            # IMPORTANT: During FGSM, we only want gradients w.r.t. the INPUT to find
            # adversarial examples. We must NOT accumulate gradients in network parameters,
            # as that would interfere with the actual training gradients computed later.
            for _ in range(self.num_steps):
                # Enable gradient computation for the current point
                current_point_var = current_point.detach().clone().requires_grad_(True)

                # Compute gradient of search_lambda with respect to input
                loss = search_lambda(current_point_var)

                # Only compute gradients if the loss requires grad
                # (may not be the case if called inside torch.no_grad())
                if loss.requires_grad:
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
                else:
                    # No gradients available, can't perform FGSM perturbation
                    gradient = torch.zeros_like(current_point_var)

                # FGSM: perturb in the direction of the gradient sign
                # The compiled loss is often -aggregation(search_lambda), so to find worst-case
                # inputs that make the training loss high, we need to minimize search_lambda.
                # When minimise=True (we want to minimize training loss), find worst violations
                # by MINIMIZING search_lambda (which after negation/aggregation gives high loss)
                sign_grad = torch.sign(gradient)

                if minimise:
                    # Find worst violations by minimizing search_lambda
                    perturbation = -epsilon * sign_grad
                else:
                    # Find best satisfactions by maximizing search_lambda
                    perturbation = epsilon * sign_grad

                # Apply perturbation and clip to bounds
                current_point = torch.clamp(
                    current_point + perturbation.detach(), lower_bound, upper_bound
                )

            # Evaluate and store the final result from this trajectory
            result = search_lambda(current_point.detach())
            results.append(torch.as_tensor(result))

        return torch.stack(results)
