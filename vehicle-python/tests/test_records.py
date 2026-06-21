"""End-to-end tests for the codegen-driven @tensor record path."""

import importlib
import sys
from pathlib import Path
from types import ModuleType
from typing import Any

import pytest

torch = pytest.importorskip("torch")

import vehicle_lang.typing as vcl
from vehicle_lang.loss import codegen
from vehicle_lang.loss import pytorch as loss_pt

_REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = _REPO_ROOT / "vehicle/tests/golden/features/tensorLike-quantifier/spec.vcl"
GOLDEN_SOURCE = Path(__file__).parent / "data" / "tensorLike_quantifier_types.py"


@pytest.fixture(scope="module")  # type: ignore[untyped-decorator]
def types(tmp_path_factory: pytest.TempPathFactory) -> ModuleType:
    """Run codegen on the test spec, import the generated module, yield it."""
    out_dir = tmp_path_factory.mktemp("types")
    out_path = out_dir / "pair_types.py"
    codegen.generate(SPEC, out_path)
    sys.path.insert(0, str(out_dir))
    try:
        return importlib.import_module("pair_types")
    finally:
        sys.path.pop(0)


def _load(types: ModuleType) -> Any:
    return loss_pt.load_specification(
        SPEC, logic=vcl.DifferentiableLogic.DL2, types=types
    )


def test_schema_materialised_as_dataclass(types: ModuleType) -> None:
    """Test that codegen emits a torch.Tensor subclass with the spec's fields in declaration order."""
    Pair = types.Pair
    assert issubclass(Pair, torch.Tensor), "Pair should subclass torch.Tensor"
    assert Pair._FIELDS == ("a", "b")
    assert Pair._FLAT_WIDTH == 2
    assert Pair._FIELD_SLOTS == {"a": (0, 1), "b": (1, 2)}


def test_pair_construction_and_field_access(types: ModuleType) -> None:
    """Test that codegen-emitted Tensor subclass supports kwarg construction + attribute access."""
    Pair = types.Pair
    x = Pair(a=1.0, b=2.0)
    assert isinstance(x, torch.Tensor)
    assert x.shape == (2,)
    assert x.a.item() == 1.0
    assert x.b.item() == 2.0


def test_property_runs_with_record_controller(types: ModuleType) -> None:
    """Test that a property accepts a controller and invokes it with Pair instances."""
    Pair = types.Pair
    decls = _load(types)
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


def test_backward_through_record_pipeline(types: ModuleType) -> None:
    """Test that backprop through a record-typed property populates parameter gradients."""
    Pair = types.Pair
    decls = _load(types)
    safe = decls["p"]

    weight = torch.tensor(1.5, requires_grad=True)

    def controller(x: Any) -> Any:
        return Pair(a=weight * x.a + 0.1, b=weight * x.b - 0.1)

    loss = safe(controller).mean()
    loss.backward()
    assert (
        weight.grad is not None
    ), "backward through record pipeline didn't populate .grad"


def test_sampler_stays_within_schema_bounds(types: ModuleType) -> None:
    """Test that the sampler draws Pair instances inside the spec's minBound/maxBound."""
    Pair = types.Pair
    decls = _load(types)
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


def test_property_receives_pair_instances_throughout(types: ModuleType) -> None:
    """Test that every controller invocation inside the sampler search receives a Pair instance."""
    Pair = types.Pair
    decls = _load(types)
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


def test_record_spec_without_types_raises() -> None:
    """Test that load_specification on a record spec without types= raises with a clear message."""
    with pytest.raises(RuntimeError, match=r"declares @tensor record"):
        loss_pt.load_specification(SPEC, logic=vcl.DifferentiableLogic.DL2)


def test_pair_is_torch_tensor(types: ModuleType) -> None:
    """Test that Pair acts as a torch.Tensor in standard ops."""
    Pair = types.Pair
    p = Pair(a=torch.tensor(1.0), b=torch.tensor(2.0))
    assert isinstance(p, torch.Tensor)
    # nn.Linear consumes the Pair directly (last dim is 2).
    out = torch.nn.Linear(2, 1)(p)
    # __torch_function__ = _disabled_torch_function_impl: output is plain Tensor.
    assert type(out) is torch.Tensor


def test_batched_pair_field_access(types: ModuleType) -> None:
    """Test that batched Pair instances expose per-field tensors with the right shape."""
    Pair = types.Pair
    batch = Pair(a=torch.zeros(4), b=torch.ones(4))
    assert batch.shape == (4, 2)
    assert batch.a.shape == (4,)
    assert torch.equal(batch.a, torch.zeros(4))
    assert torch.equal(batch.b, torch.ones(4))


def test_controller_returning_plain_tensor_is_autocast(types: ModuleType) -> None:
    """Test that a tensor-returning controller works against a record-returning spec."""
    Pair = types.Pair
    decls = _load(types)
    safe = decls["p"]

    saw_args: list[type] = []

    def controller(x: Any) -> Any:
        saw_args.append(type(x))
        return x * 2.0

    loss = safe(controller)
    assert torch.is_tensor(loss)
    assert saw_args, "controller was not invoked"
    assert all(t is Pair for t in saw_args), f"controller saw non-Pair: {set(saw_args)}"


def test_codegen_source_matches_golden() -> None:
    """Test that the codegen output for the test spec matches the checked-in golden source."""
    import io

    buf = io.StringIO()
    codegen.generate(SPEC, buf)
    assert buf.getvalue() == GOLDEN_SOURCE.read_text(), (
        f"codegen output diverged from {GOLDEN_SOURCE}; "
        f"regenerate via 'vehicle compile python-types -s {SPEC} -o {GOLDEN_SOURCE}'"
    )
