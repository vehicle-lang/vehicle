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
    def get_loss_and_witness(
        self,
        dims: vcl.Index,
        lower_bound: vcl.Tensor,
        upper_bound: vcl.Tensor,
        minimise: bool,
        search_lambda: Callable[[vcl.Tensor], vcl.Tensor],
    ) -> tuple[Float[vcl.Tensor, "1 losses"], vcl.Tensor]:
        """
        Calculates the loss based on the provided bounds and search lambda.

        Args:
            dims: The dimensions for the sampling.
            lower_bound: The lower bound tensor.
            upper_bound: The upper bound tensor.
            search_lambda: A callable representing the search lambda.
        Returns:
            Sequence[vcl.Tensor]: The computed loss as a 1D tensor. If the size is greater than 1,
            the losses will be combined by taking the maximum.
        """
        ...

    def get_loss(
        self,
        dims: vcl.Index,
        lower_bound: vcl.Tensor,
        upper_bound: vcl.Tensor,
        minimise: bool,
        search_lambda: Callable[[vcl.Tensor], vcl.Tensor],
    ) -> Float[vcl.Tensor, "1 losses"]:
        # Calls get_loss_and_witness and removes the second argument (i.e. witness)
        pass
