#!/usr/bin/env python3
"""Double-integrator reach-avoid: train a controller against STL properties.

Scenario
--------
A point mass on a line must be steered from the origin to the goal band
[9, 11] while keeping position within [0, 15]. The controller observes
[position, velocity] and outputs [acceleration]. Dynamics:

    x' = x + v * dt
    v' = v + u * dt       (dt = 0.4, 10 steps = 4 seconds)

Three properties are compiled to differentiable losses:

    stayBounded    globally[0,9]  position in [0, posMax]
    reachGoal      finally[0,9]   position in [goalLo, goalHi]
    safeUntilGoal  until[0,9]     all state dims bounded UNTIL goal reached

Training objective:

    loss = effort + weight * sum(constraint)

The compiler emits each property as a minimisation target by default —
robustness-style logics (STL) are wrapped in ``not`` so reducing the
output always pushes the property toward satisfaction. The same script
runs unchanged under any differentiable logic (DL2, Vehicle, STL).

Prerequisites
-------------
    pip install vehicle-lang[pytorch]
    # or from the repo:
    #   cd vehicle-python
    #   uv run ../examples/closedLoopSafety/train.py
    The Vehicle compiler must be on PATH.
"""

from __future__ import annotations

from pathlib import Path

import torch
from torch import nn
from vehicle_lang import DifferentiableLogic
from vehicle_lang.loss import pytorch as loss_pt

SPEC_PATH = Path(__file__).parent / "closedLoopSafety.vcl"
PROPERTIES = ["stayBounded", "reachGoal", "safeUntilGoal"]

DT = 0.4
N_STEPS = 10
EPOCHS = 300
LEARNING_RATE = 1e-3
CONSTRAINT_WEIGHT = 50.0
BANNER = "=" * 62


def build_model(seed: int = 0) -> nn.Module:
    torch.manual_seed(seed)
    return nn.Sequential(nn.Linear(2, 32), nn.ReLU(), nn.Linear(32, 1))


def dynamics(state: torch.Tensor, action: torch.Tensor) -> torch.Tensor:
    x, v = state[0], state[1]
    u = action[0]
    return torch.stack([x + v * DT, v + u * DT])


def rollout(
    controller: nn.Module, init_state: torch.Tensor
) -> tuple[torch.Tensor, torch.Tensor]:
    state = init_state
    states = [state]
    actions: list[torch.Tensor] = []
    for _ in range(N_STEPS):
        action = controller(state)
        actions.append(action)
        state = dynamics(state, action)
        states.append(state)
    return torch.stack(states), torch.stack(actions)


def compute_robustness(declarations, controller: nn.Module) -> list[torch.Tensor]:
    return [declarations[name](controller, dynamics) for name in PROPERTIES]


def evaluate(declarations, controller: nn.Module) -> dict[str, float]:
    with torch.no_grad():
        return {
            name: rob.item()
            for name, rob in zip(
                PROPERTIES, compute_robustness(declarations, controller)
            )
        }


def print_robustness(title: str, results: dict[str, float]) -> None:
    print(f"\n{BANNER}\n{title}\n{BANNER}")
    for name, rob in results.items():
        satisfied = rob <= 0
        status = "OK      " if satisfied else "VIOLATED"
        print(f"  {name:<16} {rob:+8.2f}  [{status}]")


def train(
    declarations,
    controller: nn.Module,
    init_state: torch.Tensor,
) -> None:
    optimizer = torch.optim.Adam(controller.parameters(), lr=LEARNING_RATE)
    for epoch in range(EPOCHS):
        optimizer.zero_grad()

        robustnesses = compute_robustness(declarations, controller)
        constraint_loss = torch.stack(robustnesses).sum()

        _, actions = rollout(controller, init_state)
        effort_loss = (actions**2).sum()

        loss = effort_loss + CONSTRAINT_WEIGHT * constraint_loss
        loss.backward()
        optimizer.step()

        if (epoch + 1) % 100 == 0 or epoch == 0:
            print(
                f"  epoch {epoch + 1:4d}  loss = {loss.item():.4f}"
                f"  (effort = {effort_loss.item():.2f},"
                f" constraint = {constraint_loss.item():.2f})"
            )


def print_trajectory(controller: nn.Module, init_state: torch.Tensor) -> None:
    print(f"\n{BANNER}\nLearned trajectory  [goal: 9-11, bounds: 0-15]\n{BANNER}")
    print(f"  {'step':>4}  {'pos':>7}  {'vel':>7}  {'accel':>7}")
    print(f"  {'----':>4}  {'-------':>7}  {'-------':>7}  {'-------':>7}")
    with torch.no_grad():
        states, actions = rollout(controller, init_state)
        for t in range(N_STEPS + 1):
            x, v = states[t][0].item(), states[t][1].item()
            if t == 0:
                print(f"  {t:4d}  {x:+7.2f}  {v:+7.2f}  {'':>7}")
            else:
                a = actions[t - 1][0].item()
                print(f"  {t:4d}  {x:+7.2f}  {v:+7.2f}  {a:+7.2f}")


def main() -> None:
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    torch.set_default_device(device)
    print(f"Using device: {device}")

    print(f"\n{BANNER}")
    print(f"Loading double-integrator reach-avoid specification...\n  {SPEC_PATH.name}")
    print(BANNER)
    declarations = loss_pt.load_specification(
        SPEC_PATH,
        logic=DifferentiableLogic.STL,
        declarations=PROPERTIES,
    )
    print(f"\nCompiled properties: {PROPERTIES}")
    sign_hint = "negative = satisfied"

    controller = build_model()
    init_state = torch.tensor([0.0, 0.0])

    print_robustness(
        f"Robustness BEFORE training  ({sign_hint})",
        evaluate(declarations, controller),
    )

    print(f"\n{BANNER}\nTraining ({EPOCHS} epochs, Adam lr={LEARNING_RATE})\n{BANNER}")
    train(declarations, controller, init_state)

    print_robustness(
        f"Robustness AFTER training   ({sign_hint})",
        evaluate(declarations, controller),
    )

    print_trajectory(controller, init_state)


if __name__ == "__main__":
    main()
