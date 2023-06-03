import os
from pathlib import Path

import pytest

GOLDEN_PATH = Path("vendor") / "vehicle" / "tests" / "golden" / "compile"
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob(
    os.path.join("**", "LossFunction*.json.golden")
)


@pytest.mark.parametrize(
    "loss_function_json",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[misc]
def test_loss_function_load(loss_function_json: Path) -> None:
    from vehicle_lang.loss_function import Module

    Module.from_json(loss_function_json.read_text())
