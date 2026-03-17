from pathlib import Path

import pytest
import vehicle_lang.loss._ast as vcl_ast
import vehicle_lang.typing as vcl_typing

GOLDEN_PATH = Path(__file__).parent / "data"
GOLDEN_LOSS_FUNCTION_FILES = GOLDEN_PATH.glob("*.vcl")


@pytest.mark.parametrize(
    "specification_path",
    GOLDEN_LOSS_FUNCTION_FILES,
)  # type: ignore[untyped-decorator]
def test_lossdl2_load(specification_path: Path) -> None:
    print(f"Load {specification_path}")
    if specification_path == GOLDEN_PATH / "test_quantifier_all.vcl":
        pytest.skip("Bounds-only quantifiers not yet supported in DL2 load")
    if specification_path == GOLDEN_PATH / "test_quantifier_any.vcl":
        pytest.skip("Bounds-only quantifiers not yet supported in DL2 load")
    vcl_ast.load(specification_path, target=vcl_typing.DifferentiableLogic.DL2)
