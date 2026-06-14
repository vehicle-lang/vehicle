from pathlib import Path
from typing import Any

import pytest

torch = pytest.importorskip("torch")

import vehicle_lang.typing as vcl
from vehicle_lang.loss import pytorch as loss_pt

_REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = _REPO_ROOT / "vehicle/tests/golden/features/tensorLike-quantifier/spec.vcl"


def _load() -> Any:
    return loss_pt.load_specification(SPEC, logic=vcl.DifferentiableLogic.DL2)


def test_schema_materialised_as_dataclass() -> None:
    """Test that a @tensor record schema materialises as a dataclass with the spec's fields."""
    decls = _load()
    Pair = decls["Pair"]
    assert hasattr(Pair, "__dataclass_fields__"), "Pair should be a dataclass"
    field_names = list(Pair.__dataclass_fields__.keys())
    assert field_names == ["a", "b"]


def test_pair_construction_and_field_access() -> None:
    """Test that the materialised dataclass supports field-based construction and attribute access."""
    decls = _load()
    Pair = decls["Pair"]
    x = Pair(a=1.0, b=2.0)
    assert x.a == 1.0
    assert x.b == 2.0


def test_property_runs_with_record_controller() -> None:
    """Test that a property accepts a controller and invokes it with Pair instances."""
    decls = _load()
    Pair = decls["Pair"]
    safe = decls["p"]

    received: list[type] = []

    def controller(x: Any) -> Any:
        received.append(type(x))
        return Pair(a=x.a * 2.0 + 1.0, b=x.b - 0.5)

    loss = safe(controller)
    assert torch.is_tensor(loss)
    assert received, "controller was not invoked"
    assert all(
        t is Pair for t in received
    ), f"controller must receive Pair instances, got {set(received)}"


def test_backward_through_record_pipeline() -> None:
    """Test that backprop through a record-typed property populates parameter gradients."""
    decls = _load()
    Pair = decls["Pair"]
    safe = decls["p"]

    weight = torch.tensor(1.5, requires_grad=True)

    def controller(x: Any) -> Any:
        return Pair(a=weight * x.a + 0.1, b=weight * x.b - 0.1)

    loss = safe(controller).mean()
    loss.backward()
    assert (
        weight.grad is not None
    ), "backward through record pipeline didn't populate .grad"


def test_network_returning_non_record_raises_typeerror() -> None:
    """Test that a controller returning a non-record value raises TypeError, not AttributeError."""
    decls = _load()
    safe = decls["p"]

    def bad_controller(x: Any) -> Any:
        # Spec declares f : Pair -> Pair; returning a tensor is the user error.
        return torch.stack([x.a, x.b])

    with pytest.raises(TypeError, match=r"expected a record value with field 'a'"):
        safe(bad_controller)


def test_sampler_stays_within_schema_bounds() -> None:
    """Test that the sampler draws Pair instances inside the spec's minBound/maxBound."""
    decls = _load()
    Pair = decls["Pair"]
    safe = decls["p"]

    seen: list[tuple[float, float]] = []

    weight = torch.tensor([[0.1, 0.2], [0.3, 0.4]], requires_grad=True)
    bias = torch.tensor([0.0, 0.0], requires_grad=True)

    def net(x: Any) -> Any:
        seen.append((float(x.a.detach()), float(x.b.detach())))
        flat = torch.stack([x.a, x.b])
        y = weight @ flat + bias
        return Pair(a=y[0], b=y[1])

    opt = torch.optim.Adam([weight, bias], lr=1e-2)
    for _ in range(10):
        opt.zero_grad()
        loss = safe(net).mean()
        if torch.isfinite(loss):
            loss.backward()
            opt.step()

    assert seen, "controller was not invoked"
    out_of_bounds = [
        (a, b) for a, b in seen if not (0.0 <= a <= 10.0 and 0.0 <= b <= 10.0)
    ]
    assert (
        not out_of_bounds
    ), f"sampler produced points outside [0,10]x[0,10]: first 3 = {out_of_bounds[:3]}"


def test_property_receives_pair_instances_throughout() -> None:
    """Test that every controller invocation inside the sampler search receives a Pair instance."""
    decls = _load()
    Pair = decls["Pair"]
    safe = decls["p"]

    saw_non_pair = []

    def recorder(x: Any) -> Any:
        if not isinstance(x, Pair):
            saw_non_pair.append(type(x).__name__)
        return Pair(a=x.a * 0.5, b=x.b * 0.5)

    safe(recorder)
    assert (
        not saw_non_pair
    ), f"controller saw non-Pair argument types: {set(saw_non_pair)}"
