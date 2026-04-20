"""Verify that duplicated rollout references within a property evaluate once."""

from pathlib import Path
from typing import Any, Tuple

import pytest

import vehicle_lang as vcl


def require_pytorch() -> Tuple[Any, Any]:
    torch_module = pytest.importorskip(
        "torch",
        reason="PyTorch extra is required for rollout sharing tests",
    )
    loss_module = pytest.importorskip(
        "vehicle_lang.loss.pytorch",
        reason="vehicle_lang[pytorch] extra is not installed",
    )
    pytest.importorskip(
        "vehicle_stl",
        reason="vehicle-stl is required for temporal operators in this spec",
    )
    return torch_module, loss_module


def test_rollout_shared_across_duplicate_references() -> None:
    """Property body references `positions` twice; rollout should run once."""
    torch, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_rollout_sharing.vcl"

    declarations = loss_pt.load_specification(
        spec_path,
        logic=vcl.DifferentiableLogic.STL,
        declarations=["bounded"],
    )

    controller_calls = 0

    def controller(x: Any) -> Any:
        nonlocal controller_calls
        controller_calls += 1
        return torch.zeros(1)

    def dynamics(state: Any, action: Any) -> Any:
        return state + action

    # Spec: rollout[4] => controller runs n-1 = 3 times per rollout.
    # Without sharing: two inlined rollouts * 3 = 6 calls.
    # With sharing:    one cached rollout * 3 = 3 calls.
    result = declarations["bounded"](controller, dynamics)
    assert isinstance(result, torch.Tensor)
    assert controller_calls == 3, (
        f"Expected controller to run once per step (3), got {controller_calls} — "
        "rollout sharing is broken."
    )


def test_rollout_cache_clears_between_invocations() -> None:
    """Cache must clear between calls so stale tensors don't leak after weight updates."""
    torch, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_rollout_sharing.vcl"

    declarations = loss_pt.load_specification(
        spec_path,
        logic=vcl.DifferentiableLogic.STL,
        declarations=["bounded"],
    )

    controller_calls = 0

    def controller(x: Any) -> Any:
        nonlocal controller_calls
        controller_calls += 1
        return torch.zeros(1)

    def dynamics(state: Any, action: Any) -> Any:
        return state + action

    declarations["bounded"](controller, dynamics)
    assert controller_calls == 3

    declarations["bounded"](controller, dynamics)
    assert controller_calls == 6, (
        f"Second invocation should recompute rollout; got {controller_calls} — "
        "cache is not being cleared between property calls."
    )
