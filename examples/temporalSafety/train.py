#!/usr/bin/env python3
"""Adaptive Cruise Control: training a controller to satisfy temporal STL properties.

Scenario
--------
An ego vehicle follows a lead vehicle.  The controller receives the initial
state (v_ego=15 m/s, d_rel=30 m) and outputs a predicted velocity trajectory
over 10 time steps.  Three temporal STL properties must hold:

  alwaysBelowLimit   □[0,9]  v(t) ≤ 33 m/s           (hard speed limit)
  eventuallyAtTarget ◇[0,9]  28.5 ≤ v(t) ≤ 31.5 m/s  (reach cruise speed)
  limitUntilTarget   𝜙𝒰[0,9] speed stays within limit until cruise is reached

STL robustness convention (used throughout this script)
--------------------------------------------------------
  positive value  →  property satisfied  (magnitude = margin to violation)
  negative value  →  property violated   (magnitude = distance to satisfaction)

Training loss: relu(-robustness) for each property — penalises violations,
contributes zero once the property is satisfied.

Prerequisites
-------------
  pip install vehicle-lang[pytorch]
  # or from the repo:
  #   cd vehicle-python
  #   uv run --extra pytorch python ../examples/temporalSafety/train.py
  The Vehicle compiler (cabal or a vehicle binary) must be on PATH.
"""

from __future__ import annotations

from pathlib import Path

import torch
import torch.nn as nn
from vehicle_lang import DifferentiableLogic
from vehicle_lang.loss import pytorch as loss_pt

# ---------------------------------------------------------------------------
# 1. Compile the Vehicle specification to STL loss functions
# ---------------------------------------------------------------------------

SPEC_PATH = Path(__file__).parent / "temporalSafety.vcl"
PROPS = ["alwaysBelowLimit", "eventuallyAtTarget", "limitUntilTarget"]

print("=" * 62)
print("Loading ACC specification...")
print(f"  {SPEC_PATH.name}")
print("=" * 62)

# logic=STL: temporal operators use exact min/max robustness semantics,
# automatically derived from the STLLoss DifferentiableTensorLogic defined
# in Definitions.vcl.  Each @property becomes a callable (network) -> scalar.
declarations = loss_pt.load_specification(
    SPEC_PATH,
    logic=DifferentiableLogic.STL,
    declarations=PROPS,
)

print(f"\nCompiled properties: {PROPS}")

# ---------------------------------------------------------------------------
# 2. Define a small network matching the @network declaration
#    controller : Tensor Real [2] -> Tensor Real [10]
# ---------------------------------------------------------------------------

torch.manual_seed(0)
model = nn.Sequential(
    nn.Linear(2, 16),
    nn.ReLU(),
    nn.Linear(16, 10),
)


def controller(x: torch.Tensor) -> torch.Tensor:
    """Thin wrapper satisfying the Vehicle @network interface."""
    return model(x)


# ---------------------------------------------------------------------------
# 3. Evaluate BEFORE training
#    An untrained network outputs values near zero.
#    alwaysBelowLimit   ≈ +33   (trivially satisfied — outputs << 33 m/s)
#    eventuallyAtTarget ≈ -28.5 (deeply violated — outputs never near 30 m/s)
#    limitUntilTarget   ≈ -28.5 (violated because atTarget is never true)
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Robustness BEFORE training  (positive = satisfied)")
print("=" * 62)
with torch.no_grad():
    for name in PROPS:
        rob = declarations[name](controller).item()
        status = "OK      " if rob >= 0 else "VIOLATED"
        print(f"  {name:<24} {rob:+8.2f}  [{status}]")

# ---------------------------------------------------------------------------
# 4. Training loop
#    Minimise the sum of hinge losses relu(-robustness) across all properties.
#    A satisfied property (robustness >= 0) contributes 0 to the loss.
#    The network learns to push its trajectory toward the [28.5, 31.5] band.
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Training (300 epochs, Adam lr=1e-3)")
print("=" * 62)

optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)

for epoch in range(300):
    optimizer.zero_grad()

    robs = [declarations[name](controller) for name in PROPS]
    loss = torch.stack([torch.relu(-r) for r in robs]).sum()

    loss.backward()
    optimizer.step()

    if (epoch + 1) % 100 == 0 or epoch == 0:
        print(f"  epoch {epoch + 1:4d}  loss = {loss.item():.4f}")

# ---------------------------------------------------------------------------
# 5. Evaluate AFTER training
#    All three robustness values should now be positive.
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Robustness AFTER training   (positive = satisfied)")
print("=" * 62)
with torch.no_grad():
    for name in PROPS:
        rob = declarations[name](controller).item()
        status = "OK      " if rob >= 0 else "VIOLATED"
        print(f"  {name:<24} {rob:+8.2f}  [{status}]")

# ---------------------------------------------------------------------------
# 6. Inspect the learned velocity trajectory
#    Should show the vehicle accelerating from ~15 m/s toward the 30 m/s target.
# ---------------------------------------------------------------------------

print("\n" + "=" * 62)
print("Learned velocity trace (m/s)  [target band: 28.5 – 31.5]")
print("=" * 62)
with torch.no_grad():
    trace = controller(torch.tensor([15.0, 30.0])).tolist()
for t, v in enumerate(trace):
    bar = "#" * max(0, int(v / 2))
    print(f"  step {t:2d}: {v:6.2f} m/s  {bar}")
