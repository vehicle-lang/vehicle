"""Loss-benchmark driver, tensor encoding of windController.

Runs N training iterations of a fixed 2->16->1 MLP against the tensor-style
windController spec. Wrapped by scripts/benchmark-loss.

The matching records-side driver is at ../windController-newStyle/bench_train.py
and must stay in sync (same architecture, optimiser, seed, iteration count).
"""

from __future__ import annotations

import argparse
import time
from pathlib import Path

import torch
import torch.nn as nn
import torch.optim as optim
from vehicle_lang.loss.pytorch import load_specification
import acasXu_record_types

SPEC_PATH = Path(__file__).parent / "acasXu-record.vcl"


class Controller(nn.Module):
    def __init__(self, hidden: int = 16) -> None:
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(2, hidden),
            nn.Tanh(),
            nn.Linear(hidden, 1),
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return self.net(x)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--steps", type=int, default=500)
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--quiet", action="store_true")
    args = parser.parse_args()

    torch.set_num_threads(1)
    torch.manual_seed(args.seed)

    controller = Controller()
    spec = load_specification(SPEC_PATH, types=acasXu_record_types)
    # safe = spec["safe"]
    # optimiser = optim.Adam(controller.parameters(), lr=1e-2)

    # t0 = time.perf_counter()
    # for _ in range(args.steps):
    #     optimiser.zero_grad()
    #     loss = safe(controller).mean()
    #     loss.backward()
    #     optimiser.step()
    # elapsed = time.perf_counter() - t0

    # final = safe(controller).mean().item()
    # if not args.quiet:
    #     print(
    #         f"encoding=tensors steps={args.steps} seed={args.seed} "
    #         f"final_loss={final:.6f} loop_seconds={elapsed:.4f} "
    #         f"per_step_ms={elapsed / args.steps * 1000:.3f}"
    #     )


if __name__ == "__main__":
    main()
