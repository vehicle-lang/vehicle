from __future__ import annotations

from typing import Callable, Iterable, Mapping

import tensorflow as tf

LossClosure = Callable[[], tf.Tensor]


class GradNormBalancer:
    """GradNorm (Chen et al. 2018, arXiv:1711.02257), TF variant."""

    def __init__(
        self,
        losses: Mapping[str, LossClosure],
        *,
        alpha: float = 1.5,
        shared_params: Iterable[tf.Variable] | None = None,
        model: tf.keras.Model | None = None,
        weight_lr: float = 0.025,
    ) -> None:
        if not losses:
            raise ValueError("GradNormBalancer needs at least one loss")
        if shared_params is None:
            if model is None:
                raise TypeError(
                    "GradNormBalancer requires either `shared_params` or `model`"
                )
            shared_params = list(model.trainable_variables)
        else:
            shared_params = list(shared_params)
        if not shared_params:
            raise ValueError("`shared_params` is empty")

        self._task_names: list[str] = list(losses.keys())
        self._losses: dict[str, LossClosure] = dict(losses)
        self._alpha: float = float(alpha)
        self._shared_params: list[tf.Variable] = shared_params
        self._T: int = len(self._task_names)

        self._weights: dict[str, tf.Variable] = {
            name: tf.Variable(1.0, trainable=True, name=f"gradnorm_w_{name}")
            for name in self._task_names
        }
        self._weight_optimizer = tf.keras.optimizers.Adam(learning_rate=weight_lr)
        self._initial_losses: dict[str, float] | None = None

    @property
    def weights(self) -> dict[str, tf.Tensor]:
        return {name: tf.identity(self._weights[name]) for name in self._task_names}

    def step(self) -> tuple[tf.Tensor, dict[str, tf.Tensor]]:
        weight_vars = [self._weights[name] for name in self._task_names]

        with tf.GradientTape() as outer_tape:
            with tf.GradientTape(persistent=True) as inner_tape:
                per_task: dict[str, tf.Tensor] = {
                    name: self._losses[name]() for name in self._task_names
                }

                if self._initial_losses is None:
                    self._initial_losses = {
                        name: float(per_task[name].numpy()) for name in self._task_names
                    }

                weighted: dict[str, tf.Tensor] = {
                    name: self._weights[name] * per_task[name]
                    for name in self._task_names
                }

            grad_norms: dict[str, tf.Tensor] = {}
            for name in self._task_names:
                grads = inner_tape.gradient(weighted[name], self._shared_params)
                flat = tf.concat(
                    [tf.reshape(g, [-1]) for g in grads if g is not None],
                    axis=0,
                )
                grad_norms[name] = tf.norm(flat, ord=2)

            del inner_tape

            loss_ratios = tf.stack(
                [
                    tf.stop_gradient(per_task[name]) / self._initial_losses[name]
                    for name in self._task_names
                ]
            )
            r = loss_ratios / tf.reduce_mean(loss_ratios)

            g_stack = tf.stack([grad_norms[name] for name in self._task_names])
            targets = tf.stop_gradient(
                tf.stop_gradient(tf.reduce_mean(g_stack)) * tf.pow(r, self._alpha)
            )

            l_grad = tf.reduce_sum(tf.abs(g_stack - targets))

        total_loss = tf.add_n(
            [
                tf.stop_gradient(self._weights[name]) * per_task[name]
                for name in self._task_names
            ]
        )

        w_grads = outer_tape.gradient(l_grad, weight_vars)
        self._weight_optimizer.apply_gradients(zip(w_grads, weight_vars))

        current_sum = tf.add_n(weight_vars)
        scale = tf.cast(self._T, current_sum.dtype) / current_sum
        for name in self._task_names:
            self._weights[name].assign(self._weights[name] * scale)

        return total_loss, per_task
