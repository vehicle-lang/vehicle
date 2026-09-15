from abc import ABC, abstractmethod
from typing import Callable, Generic

from jaxtyping import Float

from . import _types as vcl


class ABCSampler(
    Generic[
        vcl.Index,
        vcl.Tensor,
    ],
    ABC,
):
    @abstractmethod
    def get_loss_and_input(
        self,
        dims: vcl.Index,
        lower_bound: vcl.Tensor,
        upper_bound: vcl.Tensor,
        search_lambda: Callable[[vcl.Tensor], vcl.Tensor],
        search: bool = False,
    ) -> tuple[Float[vcl.Tensor, "1 losses"], vcl.Tensor]:
        """
        Uses gradient ascent or descent to generate samples and evaluate the search lambda.

        Args:
            dims: The dimensions for the sampling
            lower_bound: The lower bound tensor
            upper_bound: The upper bound tensor
            search_lambda: A callable representing a loss function

        Returns:
            A sequence of loss values evaluated at the PGD-perturbed points and
            the final perturbed point (out of all trajectories)
        """
        ...

    def get_loss(
        self,
        dims: vcl.Index,
        lower_bound: vcl.Tensor,
        upper_bound: vcl.Tensor,
        search_lambda: Callable[[vcl.Tensor], vcl.Tensor],
    ) -> Float[vcl.Tensor, "1 losses"]:
        """
        Calculates the loss based on the provided bounds and search lambda. Currently only
        used for training.

        Args:
            dims: The dimensions for the sampling.
            lower_bound: The lower bound tensor.
            upper_bound: The upper bound tensor.
            search_lambda: A callable representing the search lambda.
        Returns:
            Sequence[vcl.Tensor]: The computed loss as a 1D tensor. If the size is greater than 1,
            the losses will be combined by taking the maximum.
        """
        loss, _ = self.get_loss_and_input(
            dims=dims,
            lower_bound=lower_bound,
            upper_bound=upper_bound,
            search_lambda=search_lambda,
            search=False,
        )
        return loss
