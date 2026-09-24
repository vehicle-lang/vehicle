"""A specification whose compiled form names a shared monomorphised helper must still load.

Monomorphisation names such helpers `forallIndex--Real--3`, which is not a Python identifier.
The translation has to sanitise every name it emits, definitions and references alike.
"""

import pytest

from ..config import PYTHON_TEST_SPECS_PATH

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for backend tests"
)

SPEC = PYTHON_TEST_SPECS_PATH / "test_shared_helper.vcl"


def test_specification_with_shared_helper_loads_and_runs() -> None:
    import vehicle_lang as vcl
    from vehicle_lang.loss import pytorch as vpt

    declarations = vpt.load_specification(SPEC, logic=vcl.VehicleDifferentiableLogic())
    assert "p" in declarations and "twoOfThree" in declarations

    satisfied = float(
        declarations["twoOfThree"](
            f=lambda _x: torch.tensor([1.0, 2.0, 3.0]), x=torch.zeros(1)
        )
    )
    violated = float(
        declarations["twoOfThree"](
            f=lambda _x: torch.tensor([1.0, -2.0, 3.0]), x=torch.zeros(1)
        )
    )
    assert satisfied <= 0 < violated
