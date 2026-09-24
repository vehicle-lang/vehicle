"""`const v dims` with a tensor `v` must broadcast it, not scalarise it.

A specification reaches `ConstTensor` with a tensor value whenever it writes `const p dims` for a
parameter or `let m = max a b in ... const m dims`. Taking `float(v)` there drops the gradient
through `v` and cannot run under `vmap`, which the default sampler now relies on.
"""

import pytest

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)

from vehicle_lang.loss._pytorch._builtins import PyTorchBuiltins


def test_tensor_value_keeps_its_gradient() -> None:
    value = torch.tensor(2.0, requires_grad=True)
    out = PyTorchBuiltins().ConstTensor(value, (3,))
    assert out.shape == (3,)
    out.sum().backward()
    assert value.grad is not None and float(value.grad) == 3.0


def test_tensor_value_works_under_vmap() -> None:
    builtins = PyTorchBuiltins()
    batched = torch.func.vmap(lambda v: builtins.ConstTensor(v, (2,)).sum())(
        torch.arange(4.0)
    )
    torch.testing.assert_close(batched, torch.tensor([0.0, 2.0, 4.0, 6.0]))


def test_literal_value_is_unchanged() -> None:
    out = PyTorchBuiltins().ConstTensor(1.5, (2, 2))
    torch.testing.assert_close(out, torch.full((2, 2), 1.5))
