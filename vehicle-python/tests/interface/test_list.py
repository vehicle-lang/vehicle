from pathlib import Path

import pytest

import vehicle_lang as vcl

from ..config import HASKELL_GOLDEN_TESTS_PATH

GOLDEN_SPEC_FILES = (HASKELL_GOLDEN_TESTS_PATH / "specifications").glob("*/*.vcl")


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[untyped-decorator]
def test_golden_spec_list_decode(spec_path: Path) -> None:
    """
    Tests that the Python bindings for listing entities in a Vehicle specification works.
    """
    vcl.list_entities(spec_path)
