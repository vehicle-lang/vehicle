from pathlib import Path

import pytest
from vehicle_lang.error import VehicleUserError
from vehicle_lang.typecheck import typecheck

GOLDEN_PATH = (
    Path(__file__).parent.parent.parent
    / "vehicle"
    / "tests"
    / "golden"
    / "errors"
    / "typing"
)
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob("*/*.vcl")


@pytest.mark.parametrize(
    "specification_path",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[untyped-decorator]
def test_errors(specification_path: Path) -> None:
    print(f"Type-check {specification_path}")

    with pytest.raises(VehicleUserError):
        typecheck(specification_path)
