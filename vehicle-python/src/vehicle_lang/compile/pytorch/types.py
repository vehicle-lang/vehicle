from abc import ABC, abstractmethod

import torch
from typing_extensions import Callable, TypeAlias

Index: TypeAlias = int
Rat: TypeAlias = float


class ABCSampler(ABC):
    @abstractmethod
    def get_loss(
        self,
        lower_bound: torch.Tensor,
        upper_bound: torch.Tensor,
        search_lambda: Callable[[torch.Tensor], torch.Tensor],
        minimise: bool,
    ) -> torch.Tensor:
        """
        Calculates the loss based on the provided bounds and search lambda.

        Args:
            lower_bound (torch.Tensor): The lower bound tensor.
            upper_bound (torch.Tensor): The upper bound tensor.
            search_lambda (Callable[[torch.Tensor], torch.Tensor]): A callable that takes a tensor and
            minimise (bool): A flag indicating whether to minimise the loss.
        Returns:
            torch.Tensor: The computed loss tensor of size >= 1.If the size is greater than 1,
            the losses will be combined with reductionOp from the SearchRatTensor node.
        """
        ...
