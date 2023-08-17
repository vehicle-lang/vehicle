from typing import Callable

import tensorflow as tf

from ..typing import Minimise, Predicate, VariableDomain


def pgd(
    variable: tf.Variable,
    domain: VariableDomain[tf.Tensor],
    predicate: Predicate[tf.Tensor, tf.Tensor],
    minimise: Minimise,
    number_of_iterations: int = 5,
    alpha: float = 0.05,
) -> tf.Tensor:
    # Generate the starting point of the PGD attack
    variable.assign(domain.random_value())

    # Calculate which direction we're stepping the gradient.
    direction = 1 if minimise else -1

    for _ in range(number_of_iterations):
        # Calculate the loss for the current candidate value
        with tf.GradientTape() as tape:
            tape.watch(variable)
            loss = predicate(variable)

        # Get the gradients of the loss w.r.t to the input variable.
        gradient = tape.gradient(loss, variable)
        # Get the sign of the gradients to create the perturbation
        signed_grad = tf.sign(gradient)
        # Update the value of the variable
        perturbed_value = variable + direction * alpha * signed_grad
        # Clip the variable so it lies within the valid bounds.
        variable.assign(domain.clip(perturbed_value))

    return loss
