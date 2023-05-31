import json
from pathlib import Path
from typing import List


def test_loss_function_json() -> None:
    from vehicle_lang.loss_function import Declaration

    golden_dir = Path("vendor") / "vehicle" / "tests" / "golden" / "compile"
    for golden_file in golden_dir.glob("**/LossFunction.json.golden"):
        print(f"Test {golden_file}")
        value = json.loads(golden_file.read_text())
        if not isinstance(value, List):
            value = [value]
        for item in value:
            declaration = Declaration.from_dict(item)
            assert item == json.loads(declaration.to_json())
