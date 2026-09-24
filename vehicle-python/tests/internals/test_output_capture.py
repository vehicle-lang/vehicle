"""Compiling must not swallow output the process had buffered before the call.

The bindings capture the compiler's output by redirecting the process's stdout descriptor to a
pseudo-terminal. Python's own stdout is block-buffered when it is a pipe or a file, so a caller
that printed before compiling has text waiting in that buffer; flushed after the redirect, it
arrives in the capture ahead of the compiler's JSON and the load fails, and the caller's own
output is lost. `capfd` captures at the descriptor level, which is the level the bug lives at.
"""

import pytest

from ..config import PYTHON_TEST_SPECS_PATH

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)

SPEC = PYTHON_TEST_SPECS_PATH / "test_quantifier_direction.vcl"


def test_buffered_stdout_survives_a_compile(capfd: pytest.CaptureFixture[str]) -> None:
    import vehicle_lang as vcl
    from vehicle_lang.loss import pytorch as vpt

    lines = [f"training output line {i}" for i in range(300)]
    for line in lines:
        print(line)  # no flush: this is what a training loop does

    declarations = vpt.load_specification(SPEC, logic=vcl.VehicleDifferentiableLogic())
    assert "universal" in declarations

    out, _ = capfd.readouterr()
    assert all(line in out for line in lines), "buffered output was lost to the capture"
