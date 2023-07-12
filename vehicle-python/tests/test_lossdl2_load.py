import os
from pathlib import Path

import pytest

GOLDEN_PATH = Path("vendor") / "vehicle" / "tests" / "golden" / "compile"
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob(os.path.join("**", "DL2Loss.vcl.golden"))

EXCLUDE_LIST = [
    GOLDEN_PATH / "mnist-robustness" / "DL2Loss.vcl.golden",
    GOLDEN_PATH / "windController" / "DL2Loss.vcl.golden",
]


@pytest.mark.parametrize(
    "dl2loss_specification_path",
    filter(lambda spec: spec not in EXCLUDE_LIST, GOLDEN_LOSS_FUNCTION_FILES),
)  # type: ignore[misc]
def test_interpret_load(dl2loss_specification_path: Path) -> None:
    specification_path = dl2loss_specification_path.parent / "spec.vcl"
    print(f"Load {specification_path}")
    from vehicle_lang.session import Target, load

    load(specification_path, target=Target.LOSS_DL2)
