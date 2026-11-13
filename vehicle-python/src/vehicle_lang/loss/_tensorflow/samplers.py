from abc import abstractmethod
from typing import TYPE_CHECKING, Any, Callable, Sequence

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
    def get_loss_and_input(
        self,
        dims: Sequence[int],
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
    ) -> tuple[Float[tf.Tensor, "1 losses"], tf.Tensor]:
        """Validates the sampling domain and calls the core sampler implementation."""
        self._validate_domain(lower_bound, upper_bound)
        return self._get_loss_and_input(dims, lower_bound, upper_bound, search_lambda)

    def _validate_domain(
        self,
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
    ) -> None:
        """Checks that the sampling domain is non-empty."""
        tf.debugging.assert_less_equal(
            lower_bound,
            upper_bound,
            message="Empty sampling domain: lower bound exceeds upper bound.",
        )

    @abstractmethod
    def _get_loss_and_input(
        self,
        dims: Sequence[int],
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
    ) -> tuple[Float[tf.Tensor, "1 losses"], tf.Tensor]:
        """
        Runs the core sampling procedure for the specific backend.
        Uses gradient ascent or descent to generate samples and evaluate the search lambda.
        """
        ...

    @staticmethod
    def starting_region(
        lb: tf.Tensor, ub: tf.Tensor, distance: float
    ) -> tuple[tf.Tensor, tf.Tensor]:
        """
        Give each unbounded endpoint, which the domain represents as -infinity or infinity, a
        finite one `distance` away, leaving bounded endpoints alone.
        """
        offset = tf.cast(distance, tf.float32)
        low_infinite = tf.math.is_inf(lb) & (lb < 0)
        high_infinite = tf.math.is_inf(ub) & (ub > 0)
        origin = tf.zeros_like(lb)
        low_anchor = tf.where(high_infinite, origin, ub)
        high_anchor = tf.where(low_infinite, origin, lb)
        start_low = tf.where(low_infinite, low_anchor - offset, lb)
        start_high = tf.where(high_infinite, high_anchor + offset, ub)
        return start_low, start_high


class DefaultTensorFlowSampler(TensorFlowSampler):
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
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
    ) -> tuple[Float[tf.Tensor, "1 losses"], tf.Tensor]:
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
        if self.seed is not None:
            tf.random.set_seed(self.seed)

        # Ensure bounds are tensors with proper dtype
        lb = tf.cast(lower_bound, tf.float32)
        ub = tf.cast(upper_bound, tf.float32)

        # Infer step size from bounds: use a fraction of the range
        start_low, start_high = self.starting_region(
            lb, ub, self.unbounded_search_distance
        )
        range_size = tf.subtract(start_high, start_low)
        epsilon = range_size / tf.cast(self.num_steps, tf.float32)

        results = []
        points = []

        # Use multiple random starting points to ensure diversity
        for _ in range(self.num_samples):
            # Start from a random initial point in the valid range
            current_point = tf.add(
                start_low,
                tf.multiply(
                    tf.random.uniform(shape=dims, dtype=tf.float32), range_size
                ),
            )

            # Perform PGD iterations from this starting point
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

                # We are searching for the infimum of the lambda,
                # so we need to follow the gradient downards to find the most true value.
                perturbation = -epsilon * tf.sign(gradient)

                # Apply perturbation and clip to bounds
                current_point = tf.clip_by_value(current_point + perturbation, lb, ub)

            points.append(current_point)
            # Evaluate and store the final result from this trajectory
            result = search_lambda(tf.convert_to_tensor(current_point))
            results.append(tf.convert_to_tensor(result))

        return tf.stack(results), tf.stack(points)


class ConstantTensorFlowSampler(TensorFlowSampler):
    """
    A simple sampler that always returns a constant value, regardless of the input.
    Used primarily for testing purposes.
    """

    def __init__(self, constant_value: tf.Tensor, num_samples: int = 10):
        self.constant_value = constant_value
        self.num_samples = num_samples

    def _get_loss_and_input(
        self,
        dims: Sequence[int],
        lower_bound: tf.Tensor,
        upper_bound: tf.Tensor,
        search_lambda: Callable[[tf.Tensor], tf.Tensor],
    ) -> tuple[Float[tf.Tensor, "1 losses"], tf.Tensor]:
        """Returns the original constant value."""
        results = []
        points = []
        for _ in range(self.num_samples):
            result = search_lambda(self.constant_value)
            results.append(tf.convert_to_tensor(result))
            points.append(self.constant_value)
        return tf.stack(results), tf.stack(points)
