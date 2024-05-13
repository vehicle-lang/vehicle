import os
from pathlib import Path

import pytest
import vehicle_lang.ast as vcl_ast
import vehicle_lang.typing as vcl_typing

GOLDEN_PATH = Path(__file__).parent / "data"
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob("*.vcl")


@pytest.mark.parametrize(
    "specification_path",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[misc]
def test_lossdl2_load(specification_path: Path) -> None:
    print(f"Load {specification_path}")
    vcl_ast.load(specification_path, target=vcl_typing.DifferentiableLogic.DL2)
