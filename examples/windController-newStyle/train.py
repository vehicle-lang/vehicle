"""Train a small MLP against the DL2 loss for the windController.vcl spec.

Loads the spec via the PyTorch loss backend, materialises Input and Output
as dataclasses, then runs Adam over the safe property's loss.

Run from this directory:

    uv run python train.py
"""

from __future__ import annotations

import torch
import torch.nn as nn
import torch.optim as optim
from vehicle_lang.loss.pytorch import load_specification

SPEC_PATH = "windController.vcl"


class Controller(nn.Module):
    """Two-layer MLP from an Input dataclass to an Output."""

    def __init__(self, hidden: int = 16) -> None:
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(2, hidden),
            nn.Tanh(),
            nn.Linear(hidden, 1),
        )

    def forward(self, x):  # type: ignore[no-untyped-def]
        flat = torch.stack([x.currentSensor, x.previousSensor], dim=-1)
        y = self.net(flat).squeeze(-1)
        return Output(deltaVelocity=y)


if __name__ == "__main__":
    spec = load_specification(SPEC_PATH)
    Input = spec["Input"]
    Output = spec["Output"]
    safe = spec["safe"]

    torch.manual_seed(0)
    controller = Controller()
    optimiser = optim.Adam(controller.parameters(), lr=1e-2)

    for step in range(50):
        optimiser.zero_grad()
        loss = safe(controller).mean()
        loss.backward()
        optimiser.step()
        if step % 10 == 0:
            print(f"step {step:3d}  loss={loss.item():.6f}")

    final = safe(controller).mean().item()
    print(f"final loss: {final:.6f}")
