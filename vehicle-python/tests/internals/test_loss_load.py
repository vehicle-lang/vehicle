from pathlib import Path

import pytest
import vehicle_lang.loss as loss
import vehicle_lang.typing as vcl_typing

from ..config import PYTHON_TEST_SPECS_PATH


@pytest.mark.parametrize(
    "specification_path",
    PYTHON_TEST_SPECS_PATH.glob("*.vcl"),
)  # type: ignore[untyped-decorator]
def test_lossdl2_load(specification_path: Path) -> None:
    """
    These tests simply run through every specification in the test folder
    and attempt to load it. The main goal is to ensure that the AST loading
    process can handle all of the various constructs used in the specifications.
    """
    print(f"Load {specification_path}")
    if specification_path == PYTHON_TEST_SPECS_PATH / "test_quantifier_all.vcl":
        pytest.skip("Bounds-only quantifiers not yet supported in DL2 load")
    if specification_path == PYTHON_TEST_SPECS_PATH / "test_quantifier_any.vcl":
        pytest.skip("Bounds-only quantifiers not yet supported in DL2 load")
    loss.load_ast(specification_path, target=vcl_typing.DL2DifferentiableLogic())
