from abc import abstractmethod
from typing import TYPE_CHECKING, Callable, Sequence

from jaxtyping import Float

from ..._deps import require_optional_dependency
from .._abc import ABCSampler

if TYPE_CHECKING:
    import tensorflow as tf
else:  # pragma: no cover - exercised implicitly
    tf = require_optional_dependency(
        "tensorflow",
        extra="tensorflow",
        feature="The TensorFlow loss backend",
    )


class TensorFlowSampler(ABCSampler[Sequence[int], tf.Tensor]):
    @abstractmethod
    def get_loss(
        self,
        dims: Sequence[int],
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
        minimise: bool,
    ) -> Float[tf.Tensor, "1 losses"]: ...


class DefaultTensorFlowSampler(TensorFlowSampler):
    """
    Default sampler implementation for TensorFlow that uses FGSM attack.

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
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
        minimise: bool,
    ) -> Float[tf.Tensor, "1 losses"]:
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
        if self.seed is not None:
            tf.random.set_seed(self.seed)

        # Ensure bounds are tensors with proper dtype
        lb = tf.cast(lower_bound, tf.float32)
        ub = tf.cast(upper_bound, tf.float32)

        # Infer step size from bounds: use a fraction of the range
        range_size = tf.subtract(ub, lb)
        epsilon = range_size / tf.cast(self.num_steps, tf.float32)

        results = []

        # Use multiple random starting points to ensure diversity
        for _ in range(self.num_samples):
            # Start from a random initial point in the valid range
            current_point = tf.add(
                lb,
                tf.multiply(tf.random.uniform(shape=(), dtype=tf.float32), range_size),
            )

            # Perform FGSM iterations from this starting point
            for _ in range(self.num_steps):
                # Create a variable for gradient computation
                x = tf.Variable(current_point, dtype=tf.float32)

                # Compute gradient of search_lambda with respect to input
                with tf.GradientTape() as tape:
                    tape.watch(x)
                    loss = search_lambda(tf.convert_to_tensor(x))

                gradient = tape.gradient(loss, x)

                # If gradient is None or contains NaN, skip perturbation
                if gradient is None:
                    gradient = tf.zeros_like(x)
                else:
                    gradient = tf.where(
                        tf.math.is_nan(gradient), tf.zeros_like(gradient), gradient
                    )

                # FGSM: perturb in the direction of the gradient sign
                # The compiled loss is often -aggregation(search_lambda), so to find worst-case
                # inputs that make the training loss high, we need to minimize search_lambda.
                # When minimise=True (we want to minimize training loss), find worst violations
                # by MINIMIZING search_lambda (which after negation/aggregation gives high loss)
                sign_grad = tf.sign(gradient)

                if minimise:
                    # Find worst violations by minimizing search_lambda
                    perturbation = -epsilon * sign_grad
                else:
                    # Find best satisfactions by maximizing search_lambda
                    perturbation = epsilon * sign_grad

                # Apply perturbation and clip to bounds
                current_point = tf.clip_by_value(current_point + perturbation, lb, ub)

            # Evaluate and store the final result from this trajectory
            result = search_lambda(tf.convert_to_tensor(current_point))
            results.append(tf.convert_to_tensor(result))

        return tf.stack(results)
