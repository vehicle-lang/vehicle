"""`compile_specification` must build the subcommand the CLI actually has for each target.

`vehicle compile` is split into `loss`, `queries` and `itp`, each naming its target with its
own flag. Passing the target's name as if it were the subcommand fails on every call.
"""

from pathlib import Path
from typing import Any

import pytest

from vehicle_lang import compile as compile_module
from vehicle_lang.typing import ITP, DL2DifferentiableLogic, QueryFormat


@pytest.fixture
def captured(monkeypatch: pytest.MonkeyPatch) -> list[list[str]]:
    calls: list[list[str]] = []

    def fake_execute(args: Any) -> str:
        calls.append(list(args))
        return "ok"

    monkeypatch.setattr(compile_module.session, "execute_command", fake_execute)
    return calls


def test_queries_target(captured: list[list[str]]) -> None:
    compile_module.compile_specification(
        Path("spec.vcl"),
        target=QueryFormat.Marabou,
        output_file=Path("out"),
        networks={"f": Path("f.onnx")},
        parameters={"eps": 0.1},
    )
    args = captured[0]
    assert args[:4] == ["compile", "queries", "--format", "MarabouQueries"]
    assert "--network" in args and "f:f.onnx" in args
    assert "--parameter" in args and "eps:0.1" in args


def test_loss_target_takes_no_resources(captured: list[list[str]]) -> None:
    compile_module.compile_specification(
        Path("spec.vcl"),
        target=DL2DifferentiableLogic(),
        output_file=Path("out.py"),
        networks={"f": Path("f.onnx")},
    )
    args = captured[0]
    assert args[:4] == ["compile", "loss", "--logic", "DL2Loss"]
    assert "--network" not in args


def test_empty_output_is_not_an_error(monkeypatch: pytest.MonkeyPatch) -> None:
    """Compiling to a directory writes files and prints nothing."""
    monkeypatch.setattr(compile_module.session, "execute_command", lambda args: "")
    assert (
        compile_module.compile_specification(
            Path("spec.vcl"), target=QueryFormat.Marabou, output_file=Path("out")
        )
        == ""
    )


def test_itp_target(captured: list[list[str]]) -> None:
    compile_module.compile_specification(
        Path("spec.vcl"),
        target=ITP.Agda,
        output_file=Path("out.agda"),
        module_name="Spec",
    )
    args = captured[0]
    assert args[:4] == ["compile", "itp", "--target", "Agda"]
    assert "--module-name" in args
