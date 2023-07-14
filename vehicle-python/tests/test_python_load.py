import os
from pathlib import Path

import pytest

GOLDEN_PATH = Path("vendor") / "vehicle" / "tests" / "golden" / "compile"
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob(os.path.join("**", "spec.vcl"))

EXCLUDE_LIST = [
    GOLDEN_PATH / "dogsHierarchy" / "spec.vcl",
    GOLDEN_PATH / "simple-inferableParam" / "spec.vcl",
]


@pytest.mark.parametrize(
    "specification_path",
    filter(lambda spec: spec not in EXCLUDE_LIST, GOLDEN_LOSS_FUNCTION_FILES),
)  # type: ignore[misc]
def test_interpret_load(specification_path: Path) -> None:
    print(f"Load {specification_path}")
    from vehicle_lang.session import Target, load

    load(specification_path, target=Target.DEFAULT)
