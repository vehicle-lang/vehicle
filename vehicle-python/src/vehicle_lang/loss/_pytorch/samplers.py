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


@dataclass
class Sample:
    inputs: dict[str, torch.Tensor]
    loss: float
    loss_history: List[float]


class PyTorchSampler(ABCSampler[Sequence[int], torch.Tensor]):
    @abstractmethod
    def get_loss(
        self,
        dims: Sequence[int],
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
    ) -> Float[torch.Tensor, "1 losses"]: ...

    """
    @abstractmethod
    def get_samples(
        self,
        bound_vars: Sequence[BoundVarData],
        loss_fn: Callable[..., torch.Tensor],
    ) -> Sequence[Sample]: ...
    """

    @abstractmethod
    def pgd(
        self,
        bound_vars: Sequence[BoundVarData],
        loss_fn: Callable[..., torch.Tensor],
    ) -> Sample: ...


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

            # Perform PGD iterations from this starting point
            # IMPORTANT: During PGD, we only want gradients w.r.t. the INPUT to find
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
                # To find worst-case inputs that make the loss high, we need to
                # move in the direction of the gradient (gradient ascent).
                perturbation = epsilon * torch.sign(gradient)

                # Apply perturbation and clip to bounds
                current_point = torch.clamp(
                    current_point + perturbation.detach(), lower_bound, upper_bound
                )

            # Evaluate and store the final result from this trajectory
            result = search_lambda(current_point.detach())
            results.append(torch.as_tensor(result))

        return torch.stack(results)

    '''
    def get_samples(
        self,
        bound_vars: Sequence[BoundVarData],
        loss_fn: Callable[..., torch.Tensor],
    ) -> Sequence[Sample]:
        """
        Generates a sequence of samples. Each sample is a witness obtained using PGD.

        Args:
            bound_vars: Contains the name, lower bound and upper bound of each bound
                variable to search
            loss_fn: A callable representing the loss function to minimise
            num_samples: The number of witnesses to generate
            num_steps: The number of steps to take when searching each bound variable

        Returns:
        A sequence of Sample objects representing witnesses.
        """
        if self.seed is not None:
            torch.manual_seed(self.seed)

        samples = []
        for _ in range(self.num_samples):
            sample = self.pgd(bound_vars, loss_fn)
            samples.append(sample)

        return samples
    '''

    def pgd(
        self,
        bound_vars: Sequence[BoundVarData],
        loss_fn: Callable[..., torch.Tensor],
    ) -> Sample:
        """
        Uses PGD to generate a single witness. A round-robin approach is used to find
        an optimal input for each bound variable in turn.

        Uses a similar algorithm as `get_loss` except each step minimises the loss
        function. (can be unified/improved in future)

        Args:
            bound_vars: Contains the name, lower bound and upper bound of each bound
                variable to search
            loss_fn: A callable representing the loss function to minimise
            num_steps: The number of steps to take when searching each bound variable

        Returns:
        A Sample object representing a witness with the input for each bound variable,
        its loss value, and the loss values at each step in generating the witness.
        """

        # Set starting points for all bound variables
        current_inputs = {}
        for bound_var in bound_vars:
            upper_bound = bound_var.upper_bound
            lower_bound = bound_var.lower_bound
            range_size = upper_bound - lower_bound

            initial_point = (
                lower_bound + torch.rand((), dtype=lower_bound.dtype) * range_size
            )
            current_inputs[bound_var.name] = initial_point

        loss_history = []
        # Find an optimal input for each bound variable one at a time while keeping all other
        # inputs constant
        for bound_var in bound_vars:
            upper_bound = bound_var.upper_bound
            lower_bound = bound_var.lower_bound
            epsilon = (upper_bound - lower_bound) / self.num_steps

            for _ in range(self.num_steps):
                current_point = (
                    current_inputs[bound_var.name].detach().clone().requires_grad_(True)
                )
                current_inputs[bound_var.name] = current_point

                loss = loss_fn(**current_inputs)
                loss_history.append(loss.item())

                if loss.requires_grad:
                    gradient = torch.autograd.grad(
                        loss,
                        current_point,
                        create_graph=False,
                        retain_graph=False,
                        only_inputs=True,
                    )[0]

                    if gradient is not None:
                        gradient = torch.where(
                            torch.isnan(gradient), torch.zeros_like(gradient), gradient
                        )
                    else:
                        gradient = torch.zeros_like(current_point)
                else:
                    gradient = torch.zeros_like(current_point)

                sign_grad = torch.sign(gradient)
                # -epsilon * sign_grad because to search for witnesses, the loss must be minimised
                perturbation = -epsilon * sign_grad

                perturbed_point = torch.clamp(
                    current_point + perturbation.detach(), lower_bound, upper_bound
                ).detach()
                current_inputs[bound_var.name] = perturbed_point

        final_loss = loss_fn(**current_inputs)
        return Sample(
            inputs=current_inputs, loss=final_loss.item(), loss_history=loss_history
        )
