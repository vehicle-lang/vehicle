import os
from pathlib import Path

import pytest

import vehicle_lang as vcl
import vehicle_lang.ast as vcla

GOLDEN_PATH = (
    Path(__file__).parent.parent / "vendor" / "vehicle" / "tests" / "golden" / "compile"
)
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob(os.path.join("**", "DL2Loss.vcl.golden"))


@pytest.mark.parametrize(
    "dl2loss_specification_path",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[misc]
def test_interpret_load(dl2loss_specification_path: Path) -> None:
    specification_path = dl2loss_specification_path.parent / "spec.vcl"
    print(f"Load {specification_path}")
    vcla.load(
        specification_path,
        target=vcl.DifferentiableLogic.DL2,
    )
