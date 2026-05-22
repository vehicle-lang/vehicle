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

        # FGSM vectorised over the num_samples restarts (bit-identical to
        # the per-sample loop; same RNG-stream consumption).
        n = self.num_samples
        frac = torch.rand((n,) + (1,) * lower_bound.ndim, dtype=lower_bound.dtype)
        current_points = lower_bound + frac * range_size  # [n, *bound.shape]

        def _scalar_loss(x: torch.Tensor) -> torch.Tensor:
            return search_lambda(x).reshape(())

        _per_sample_grad = torch.func.grad(_scalar_loss)

        for _ in range(self.num_steps):
            cp = current_points.detach()
            # `loss.requires_grad` in the original is False only when called
            # under torch.no_grad(); mirror that as a zero perturbation.
            if torch.is_grad_enabled():
                try:
                    gradient = torch.vmap(_per_sample_grad)(cp)
                    gradient = torch.where(
                        torch.isnan(gradient),
                        torch.zeros_like(gradient),
                        gradient,
                    )
                except RuntimeError:
                    gradient = torch.zeros_like(cp)
            else:
                gradient = torch.zeros_like(cp)

            sign_grad = torch.sign(gradient)
            # minimise=True: worst violations by minimising search_lambda.
            perturbation = (-epsilon if minimise else epsilon) * sign_grad
            current_points = torch.clamp(
                cp + perturbation.detach(), lower_bound, upper_bound
            )

        # Final eval is a plain per-sample loop, not vmap: torch.func
        # transforms don't connect to closed-over autograd leaves, so
        # vmapping here would sever d_loss/d_theta and break GradNorm.
        final = current_points.detach()
        return torch.stack([torch.as_tensor(search_lambda(final[i])) for i in range(n)])
