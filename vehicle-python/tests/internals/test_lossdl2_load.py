from pathlib import Path

import pytest

import vehicle_lang.loss as loss
import vehicle_lang.typing as vcl_typing

from ..config import PYTHON_TEST_SPECS_PATH

GOLDEN_LOSS_FUNCTION_FILES = PYTHON_TEST_SPECS_PATH.glob("*.vcl")


@pytest.mark.parametrize(
    "specification_path",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[untyped-decorator]
def test_lossdl2_load(specification_path: Path) -> None:
    print(f"Load {specification_path}")
    if specification_path == PYTHON_TEST_SPECS_PATH / "test_quantifier_all.vcl":
        pytest.skip("Bounds-only quantifiers not yet supported in DL2 load")
    if specification_path == PYTHON_TEST_SPECS_PATH / "test_quantifier_any.vcl":
        pytest.skip("Bounds-only quantifiers not yet supported in DL2 load")
    if specification_path == PYTHON_TEST_SPECS_PATH / "test_multiproperty.vcl":
        pytest.skip("Multi-properties not yet supported in DL2 load")
    loss.load_ast(specification_path, target=vcl_typing.DifferentiableLogic.DL2)
