from __future__ import annotations

from typing import Callable, Iterable, Mapping

import torch
from torch import nn

LossClosure = Callable[[], torch.Tensor]


class GradNormBalancer:
    """GradNorm (Chen et al. 2018, arXiv:1711.02257)."""

    def __init__(
        self,
        losses: Mapping[str, LossClosure],
        *,
        alpha: float = 1.5,
        shared_params: Iterable[torch.Tensor] | None = None,
        model: nn.Module | None = None,
        weight_lr: float = 0.025,
    ) -> None:
        if not losses:
            raise ValueError("GradNormBalancer needs at least one loss")
        if shared_params is None:
            if model is None:
                raise TypeError(
                    "GradNormBalancer requires either `shared_params` or `model`"
                )
            shared_params = list(model.parameters())
        else:
            shared_params = list(shared_params)
        if not shared_params:
            raise ValueError("`shared_params` is empty")

        self._task_names: list[str] = list(losses.keys())
        self._losses: dict[str, LossClosure] = dict(losses)
        self._alpha: float = float(alpha)
        self._shared_params: list[torch.Tensor] = shared_params
        self._T: int = len(self._task_names)

        self._weights = nn.ParameterDict(
            {name: nn.Parameter(torch.ones(())) for name in self._task_names}
        )
        self._weight_optimizer = torch.optim.Adam(
            self._weights.parameters(), lr=weight_lr
        )
        self._initial_losses: dict[str, float] | None = None

    @property
    def weights(self) -> dict[str, torch.Tensor]:
        return {name: self._weights[name].detach().clone() for name in self._task_names}

    def step(self) -> tuple[torch.Tensor, dict[str, torch.Tensor]]:
        per_task: dict[str, torch.Tensor] = {
            name: self._losses[name]() for name in self._task_names
        }

        if self._initial_losses is None:
            self._initial_losses = {
                name: float(per_task[name].detach().item()) for name in self._task_names
            }

        # G_i = ||grad_theta(w_i L_i)|| = |w_i| * ||grad_theta L_i||
        # (w_i scalar): first-order norm * |w_i| analytically, instead of
        # a second-order graph. Algebraically identical, far cheaper.
        grad_norms: dict[str, torch.Tensor] = {}
        for name in self._task_names:
            grads = torch.autograd.grad(
                per_task[name],
                self._shared_params,
                retain_graph=True,
                create_graph=False,
                allow_unused=True,
            )
            flat = torch.cat([g.reshape(-1) for g in grads if g is not None])
            n_i = torch.linalg.vector_norm(flat, ord=2).detach()
            grad_norms[name] = self._weights[name].abs() * n_i

        loss_ratios = torch.stack(
            [
                per_task[name].detach() / self._initial_losses[name]
                for name in self._task_names
            ]
        )
        r = loss_ratios / loss_ratios.mean()

        g_stack = torch.stack([grad_norms[name] for name in self._task_names])
        targets = (g_stack.mean() * r.pow(self._alpha)).detach()

        l_grad = (g_stack - targets).abs().sum()

        # .item() not .detach(): the in-place updates below would bump
        # the version counter on a detached view and break backward().
        total_loss = torch.stack(
            [
                self._weights[name].detach().item() * per_task[name]
                for name in self._task_names
            ]
        ).sum()

        self._weight_optimizer.zero_grad()
        l_grad.backward(retain_graph=True)
        self._weight_optimizer.step()

        with torch.no_grad():
            current_sum = sum(self._weights[name] for name in self._task_names)
            scale = self._T / current_sum
            for name in self._task_names:
                self._weights[name].mul_(scale)

        return total_loss, per_task
