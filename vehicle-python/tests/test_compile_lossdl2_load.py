import os
from pathlib import Path

import pytest

GOLDEN_PATH = Path("vendor") / "vehicle" / "tests" / "golden" / "compile"
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob(os.path.join("**", "spec.vcl"))


@pytest.mark.parametrize(
    "interpret_json",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[misc]
def test_interpret_load(interpret_json: Path) -> None:
    from vehicle_lang.compile import Program

    Program.from_json(interpret_json.read_text())
