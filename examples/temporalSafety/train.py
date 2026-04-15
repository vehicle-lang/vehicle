#!/usr/bin/env python3
"""Example: compiling and evaluating temporal safety properties with Vehicle.

This script demonstrates how to:
1. Load a Vehicle specification containing temporal operators (globally, finally, until).
2. Compile the specification into differentiable PyTorch loss functions.
3. Evaluate the loss functions with a simple neural network.
4. Use temporal losses in a training loop to enforce temporal safety properties.
5. Optionally use custom temporal semantics for smoother gradients.

Prerequisites:
    pip install vehicle-lang[pytorch]
    # or, if using uv from the Vehicle repo:
    # uv run --extra pytorch python examples/temporalSafety/train.py

The Vehicle compiler (cabal-built or installed) must be available on PATH,
or you can run via `cabal run vehicle` from the repo root.
"""

from __future__ import annotations

from pathlib import Path

import torch
import torch.nn as nn
from vehicle_lang.loss import pytorch as loss_pt
from vehicle_stl import logsumexp

# ---------------------------------------------------------------------------
# Step 1: Load the temporal specification
# ---------------------------------------------------------------------------


SPEC_PATH = Path(__file__).parent / "temporalSafety.vcl"

print("=" * 60)
print("Loading Vehicle specification with temporal operators...")
print(f"  Spec: {SPEC_PATH}")
print("=" * 60)

# load_specification compiles the .vcl file to a loss-function IR via the
# Vehicle compiler, then translates it to executable PyTorch code.
# Each @property in the spec becomes a callable entry in the returned dict.
declarations = loss_pt.load_specification(SPEC_PATH, temporal_semantics=logsumexp())

print(f"\nCompiled declarations: {list(declarations.keys())}")

# ---------------------------------------------------------------------------
# Step 2: Define a simple network matching the @network declaration
# ---------------------------------------------------------------------------

# The spec declares:  @network controller : Tensor Real [4] -> Tensor Real [4]
# So our network must accept a [4]-tensor and return a [4]-tensor.

model = nn.Sequential(
    nn.Linear(4, 16),
    nn.ReLU(),
    nn.Linear(16, 4),
)


def controller(x: torch.Tensor) -> torch.Tensor:
    """Wrapper matching the Vehicle @network signature."""
    return model(x)


# ---------------------------------------------------------------------------
# Step 3: Evaluate each temporal property
# ---------------------------------------------------------------------------

print("\n" + "=" * 60)
print("Evaluating temporal properties (before training)")
print("=" * 60)

property_names = ["alwaysPositive", "eventuallyPositive", "respondsInTime"]

for name in property_names:
    if name not in declarations:
        print(f"  {name}: not found in compiled declarations")
        continue

    # Each compiled property expects the @network callable as its argument.
    result = declarations[name](controller)
    print(f"  {name} = {result}")
    print(f"    (more positve = less satisfied, 0 is fully satisfied)")

# ---------------------------------------------------------------------------
# Step 4: Training loop using temporal losses
# ---------------------------------------------------------------------------

print("\n" + "=" * 60)
print("Training to satisfy temporal safety properties")
print("=" * 60)

optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
num_epochs = 200

for epoch in range(num_epochs):
    optimizer.zero_grad()

    # Compute the temporal safety loss for each property.
    # Vehicle's loss convention: higher = more satisfied, lower = more violated.
    # We negate to get a minimisation objective.
    total_loss = torch.tensor(0.0)
    for name in property_names:
        if name in declarations:
            total_loss += declarations[name](controller)

    if (epoch + 1) % 50 == 0 or epoch == 0:
        print(f"  Epoch {epoch + 1:4d}  loss = {total_loss.item():.4f}")

    total_loss.backward()
    optimizer.step()


# ---------------------------------------------------------------------------
# Step 5: Evaluate again after training
# ---------------------------------------------------------------------------

print("\n" + "=" * 60)
print("Evaluating temporal properties (after training)")
print("=" * 60)

for name in property_names:
    if name in declarations:
        result = declarations[name](controller)
        print(f"  {name} = {result.item():.4f} (after training)")
