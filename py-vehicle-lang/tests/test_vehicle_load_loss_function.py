import json
from pathlib import Path
from typing import List

import pytest

GOLDEN_PATH = Path("vendor") / "vehicle" / "tests" / "golden" / "compile"


@pytest.mark.parametrize(
    "loss_function_json", GOLDEN_PATH.glob("**/LossFunction.json.golden")
)  # type: ignore[misc]
def test_load_loss_function(loss_function_json: Path) -> None:
    from vehicle_lang.loss_function import Declaration

    value = json.loads(loss_function_json.read_text())
    if not isinstance(value, List):
        value = [value]
    for item in value:
        declaration = Declaration.from_dict(item)
        # assert item == json.loads(declaration.to_json())
