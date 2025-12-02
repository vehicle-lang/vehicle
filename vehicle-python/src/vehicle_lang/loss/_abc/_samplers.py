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
    def get_loss(
        self,
        dims: vcl.Index,
        lower_bound: vcl.Tensor,
        upper_bound: vcl.Tensor,
        search_lambda: Callable[[vcl.Tensor], vcl.Tensor],
        minimise: bool,
    ) -> Float[vcl.Tensor, "1 losses"]:
        """
        Calculates the loss based on the provided bounds and search lambda.

        Args:
            dims: The dimensions for the sampling.
            lower_bound: The lower bound tensor.
            upper_bound: The upper bound tensor.
            search_lambda: A callable representing the search lambda.
            minimise: A flag indicating whether to minimise the search_lambda.
        Returns:
            Sequence[vcl.Tensor]: The computed loss as a 1D tensor. If the size is greater than 1,
            the losses will be combined with reductionOp from the SearchRatTensor node.
        """
        ...
