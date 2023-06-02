from typing import Any, Dict, List

import pytest

import vehicle_lang.loss_function._ast as vcl
from vehicle_lang.loss_function.translation.python import PythonTranslation


@pytest.mark.parametrize(
    "declarations,declaration_context",
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
                    vcl.Addition(vcl.Variable("one"), vcl.Constant(1)),
                ),
            ],
            {"one": 1, "two": 2},
        ),
    ],
)  # type: ignore[misc]
def test_loss_function_exec(
    declarations: List[vcl.Declaration],
    declaration_context: Dict[str, Any],
) -> None:
    compiler = PythonTranslation()
    result = compiler.compile(vcl.Module(declarations), "<string>")
    for key in declaration_context.keys():
        assert declaration_context[key] == result.get(key)
