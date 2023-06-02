import json
from pathlib import Path
from typing import List

import pytest

GOLDEN_PATH = Path("vendor") / "vehicle" / "tests" / "golden" / "compile"
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob("**/LossFunction*.json.golden")


@pytest.mark.parametrize(
    "loss_function_json",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[misc]
def test_load_loss_function(loss_function_json: Path) -> None:
    from vehicle_lang.loss_function import Module

    print(f"Load {loss_function_json}")
    Module.from_json(loss_function_json.read_text())
