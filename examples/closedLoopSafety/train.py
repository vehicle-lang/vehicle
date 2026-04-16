#!/usr/bin/env python3
"""Double-Integrator Reach-Avoid: training a controller via STL robustness.

Scenario
--------
A point mass on a line (the double integrator) must be steered from the origin
to a goal region [9, 11] while keeping position within [0, 15].  The controller
observes [position, velocity] and outputs [acceleration].  The dynamics are:

    x' = x + v * dt
    v' = v + u * dt       (dt = 0.4, so 10 steps = 4 seconds)

Two STL properties are compiled to differentiable robustness losses:

    stayBounded  =  forall i . 0 <= x(i) <= 15       (safety)
    reachGoal    =  exists i . 9 <= x(i) <= 11       (liveness)

The training objective combines a traditional task loss (control effort
minimisation) with the STL constraint penalties:

    loss = effort_loss + weight * sum(relu(-robustness))

STL robustness: positive = satisfied, negative = violated.

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
import torch.nn as nn
from vehicle_lang import DifferentiableLogic
from vehicle_lang.loss import pytorch as loss_pt

# ---------------------------------------------------------------------------
# 0. Device setup — use GPU if available
# ---------------------------------------------------------------------------

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
torch.set_default_device(device)
print(f"Using device: {device}")

# ---------------------------------------------------------------------------
# 1. Compile the Vehicle specification
# ---------------------------------------------------------------------------

SPEC_PATH = Path(__file__).parent / "closedLoopSafety.vcl"
PROPS = ["stayBounded", "reachGoal"]

print("\n" + "=" * 62)
print("Loading double-integrator reach-avoid specification...")
print(f"  {SPEC_PATH.name}")
print("=" * 62)

declarations = loss_pt.load_specification(
    SPEC_PATH,
    logic=DifferentiableLogic.STL,
    declarations=PROPS,
)

print(f"\nCompiled properties: {PROPS}")

# ---------------------------------------------------------------------------
# 2. Define the controller network
#    controller : Tensor Real [2] -> Tensor Real [1]
#    Maps [position, velocity] to [acceleration].
# ---------------------------------------------------------------------------

torch.manual_seed(0)
model = nn.Sequential(
    nn.Linear(2, 32),
    nn.ReLU(),
    nn.Linear(32, 1),
)


def controller(x: torch.Tensor) -> torch.Tensor:
    """Neural controller: [position, velocity] -> [acceleration]."""
    return model(x)


# ---------------------------------------------------------------------------
# 3. Define the dynamics (double integrator, dt = 0.2)
#    dynamics : Tensor Real [2] -> Tensor Real [1] -> Tensor Real [2]
# ---------------------------------------------------------------------------

DT = 0.4
N_STEPS = 10


def dynamics(state: torch.Tensor, action: torch.Tensor) -> torch.Tensor:
    """Double integrator: x' = x + v*dt, v' = v + u*dt."""
    x = state[0]
    v = state[1]
    u = action[0]
    x_new = x + v * DT
    v_new = v + u * DT
    return torch.stack([x_new, v_new])


def rollout_trajectory(ctrl, dyn, init):
    """Run the closed loop manually to compute control effort."""
    states = [init]
    actions = []
    state = init
    for _ in range(N_STEPS):
        action = ctrl(state)
        actions.append(action)
        state = dyn(state, action)
        states.append(state)
    return torch.stack(states), torch.stack(actions)


# ---------------------------------------------------------------------------
# 4. Evaluate BEFORE training
#    stayBounded: likely satisfied (near origin, small actions)
#    reachGoal:   violated (position never reaches [9, 11])
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Robustness BEFORE training  (positive = satisfied)")
print("=" * 62)
with torch.no_grad():
    for name in PROPS:
        rob = declarations[name](controller, dynamics).item()
        status = "OK      " if rob >= 0 else "VIOLATED"
        print(f"  {name:<16} {rob:+8.2f}  [{status}]")

# ---------------------------------------------------------------------------
# 5. Training loop
#    Combined loss = effort + weight * constraint_penalty
#    - effort: sum of squared accelerations (traditional task loss)
#    - constraint: relu(-robustness) for each STL property
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Training (300 epochs, Adam lr=1e-3)")
print("=" * 62)

CONSTRAINT_WEIGHT = 50.0
INIT_STATE = torch.tensor([0.0, 0.0])

optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)

for epoch in range(300):
    optimizer.zero_grad()

    # STL constraint losses from Vehicle
    robs = [declarations[name](controller, dynamics) for name in PROPS]
    constraint_loss = torch.stack([torch.relu(-r) for r in robs]).sum()

    # Traditional task loss: minimise control effort
    _, actions = rollout_trajectory(controller, dynamics, INIT_STATE)
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

# ---------------------------------------------------------------------------
# 6. Evaluate AFTER training
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Robustness AFTER training   (positive = satisfied)")
print("=" * 62)
with torch.no_grad():
    for name in PROPS:
        rob = declarations[name](controller, dynamics).item()
        status = "OK      " if rob >= 0 else "VIOLATED"
        print(f"  {name:<16} {rob:+8.2f}  [{status}]")

# ---------------------------------------------------------------------------
# 7. Inspect the learned trajectory
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Learned trajectory  [goal: 9-11, bounds: 0-15]")
print("=" * 62)
print(f"  {'step':>4}  {'pos':>7}  {'vel':>7}  {'accel':>7}")
print(f"  {'----':>4}  {'-------':>7}  {'-------':>7}  {'-------':>7}")
with torch.no_grad():
    states, actions = rollout_trajectory(controller, dynamics, INIT_STATE)
    for t in range(N_STEPS + 1):
        x, v = states[t][0].item(), states[t][1].item()
        if t == 0:
            print(f"  {t:4d}  {x:+7.2f}  {v:+7.2f}  {'':>7}")
        else:
            a = actions[t - 1][0].item()
            print(f"  {t:4d}  {x:+7.2f}  {v:+7.2f}  {a:+7.2f}")
