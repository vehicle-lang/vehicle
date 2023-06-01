from typing import Any, Dict, List

import pytest
import vehicle_lang.loss_function._ast as vcl
from vehicle_lang.loss_function.translation.python import PythonTranslation


@pytest.mark.parametrize(
    "decls,decl_ctx",
    [
        (
            [
                vcl.DefFunction(
                    "one",
                    vcl.Constant(1),
                ),
            ],
            {"one": 1},
        ),
        (
            [
                vcl.DefFunction(
                    "one",
                    vcl.Constant(1),
                ),
                vcl.DefFunction(
                    "two",
                    vcl.BinaryOperator(
                        "addition", vcl.Variable("one"), vcl.Constant(1)
                    ),
                ),
            ],
            {"one": 1, "two": 2},
        ),
    ],
)  # type: ignore[misc]
def test_exec_loss_function(
    decls: List[vcl.Declaration], decl_ctx: Dict[str, Any]
) -> None:
    compiler = PythonTranslation()
    result = compiler.compile_module(vcl.Module(decls), "<string>")
    for key in decl_ctx.keys():
        assert decl_ctx[key] == result.get(key)
