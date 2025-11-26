from abc import ABC, abstractmethod

import tensorflow as tf
from typing_extensions import Callable, TypeAlias

Index: TypeAlias = int
Rat: TypeAlias = float


class ABCSampler(ABC):
    @abstractmethod
    def get_loss(
        self,
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
        minimise: bool,
    ) -> tf.Tensor:
        """
        Calculates the loss based on the provided bounds and search lambda.

        Args:
            lower_bound (tf.Tensor): The lower bound tensor.
            upper_bound (tf.Tensor): The upper bound tensor.
            search_lambda (Callable[[tf.Tensor], tf.Tensor]): A callable that takes a tensor and
            minimise (bool): A flag indicating whether to minimise the loss.
        Returns:
            tf.Tensor: The computed loss tensor of size >= 1.If the size is greater than 1,
            the losses will be combined with reductionOp from the SearchRatTensor node.
        """
        ...
