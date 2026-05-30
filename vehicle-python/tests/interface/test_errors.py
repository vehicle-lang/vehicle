from pathlib import Path

import pytest
from vehicle_lang.error import VehicleUserError
from vehicle_lang.list import list_entities

from ..config import HASKELL_GOLDEN_TESTS_PATH

GOLDEN_SPECS = (HASKELL_GOLDEN_TESTS_PATH / "errors" / "typing").glob("*/*.vcl")


@pytest.mark.parametrize(
    "specification_path",
    GOLDEN_SPECS,
)  # type: ignore[untyped-decorator]
def test_errors(specification_path: Path) -> None:
    """
    Tests that the Python bindings successfully catch and propagate user errors from Vehicle.
    """
    print(f"Type-check {specification_path}")

    with pytest.raises(VehicleUserError):
        list_entities(specification_path)
