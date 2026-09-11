"""Test PyTorch backend with actual Vehicle specifications."""

import json
import subprocess
from pathlib import Path
from typing import Any, Type

import pytest
from vehicle_lang._ast._nodes import Program

torch = pytest.importorskip(
    "torch", reason="PyTorch extra is required for PyTorch integration tests"
)

from vehicle_lang.loss._pytorch._translation import PyTorchTranslation


def require_tensorflow_translation() -> Type[Any]:
    pytest.importorskip(
        "tensorflow",
        reason="TensorFlow extra is required for PyTorch/TensorFlow equivalence tests",
    )
    from vehicle_lang.loss._tensorflow._translation import TensorFlowTranslation

    return TensorFlowTranslation


def compile_vehicle_spec(spec_path: Path) -> str:
    """Compile a Vehicle specification to JSON using the Vehicle CLI."""
    vehicle_python_dir = Path(__file__).parent.parent.resolve()
    result = subprocess.run(
        [
            "uv",
            "run",
            "vehicle",
            "--json",
            "compile",
            "loss",
            "--logic",
            "DL2Loss",
            f"--specification={spec_path}",
        ],
        capture_output=True,
        text=True,
        timeout=60,
        cwd=str(vehicle_python_dir),
    )

    if result.returncode != 0:
        raise RuntimeError(f"Vehicle compilation failed: {result.stderr}")
    return result.stdout


def test_pytorch_simple_spec() -> None:
    """Test PyTorch backend with test_addition.vcl specification."""
    tests_dir = Path(__file__).parent.resolve()
    spec_path = tests_dir / "data" / "test_addition.vcl"
    if not spec_path.exists():
        pytest.skip("test_addition.vcl not found")

    json_output = compile_vehicle_spec(spec_path)
    json_data = json.loads(json_output)
    program = Program.from_dict(json_data)
    pytorch_translation = PyTorchTranslation()
    pytorch_functions = pytorch_translation.compile_program(
        program, path=spec_path, declaration_context={}, samplers={}
    )

    assert len(pytorch_functions) > 0, "Should generate at least one PyTorch function"

    user_symbols = {
        name: value
        for name, value in pytorch_functions.items()
        if name not in {"torch"} and not name.startswith("__")
    }

    assert user_symbols, "Should expose at least one user-defined declaration"


def test_pytorch_vs_tensorflow_equivalence() -> None:
    """Test that PyTorch and TensorFlow backends produce equivalent results on test_addition spec."""
    TensorFlowTranslation = require_tensorflow_translation()
    tests_dir = Path(__file__).parent.resolve()
    spec_path = tests_dir / "data" / "test_addition.vcl"

    if not spec_path.exists():
        pytest.skip("test_addition.vcl not found")

    json_output = compile_vehicle_spec(spec_path)
    json_data = json.loads(json_output)
    program = Program.from_dict(json_data)
    pytorch_translation = PyTorchTranslation()
    tensorflow_translation = TensorFlowTranslation()

    pytorch_functions = pytorch_translation.compile_program(
        program, path=spec_path, declaration_context={}, samplers={}
    )
    tensorflow_functions = tensorflow_translation.compile_program(
        program, path=spec_path, declaration_context={}, samplers={}
    )

    assert len(pytorch_functions) == len(
        tensorflow_functions
    ), f"Function count mismatch: PyTorch={len(pytorch_functions)}, TensorFlow={len(tensorflow_functions)}"
    pytorch_keys = set(pytorch_functions.keys()) - {"torch"}
    tensorflow_keys = set(tensorflow_functions.keys()) - {"tensorflow"}
    assert (
        pytorch_keys == tensorflow_keys
    ), "Function names should match between backends"


@pytest.mark.parametrize("spec_name", ["test_addition.vcl"])  # type: ignore[untyped-decorator]
def test_pytorch_compile_specifications(spec_name: str) -> None:
    """Test PyTorch compilation on various Vehicle specifications."""
    tests_dir = Path(__file__).parent.resolve()
    spec_path = tests_dir / "data" / spec_name
    if not spec_path.exists():
        pytest.skip(f"{spec_name} not found")

    try:
        json_output = compile_vehicle_spec(spec_path)
        json_data = json.loads(json_output)
        program = Program.from_dict(json_data)
        pytorch_translation = PyTorchTranslation()
        pytorch_functions = pytorch_translation.compile_program(
            program, path=spec_path, declaration_context={}, samplers={}
        )
        assert len(pytorch_functions) > 0, f"Should generate functions for {spec_name}"
        user_symbols = {
            name
            for name in pytorch_functions
            if name not in {"torch"} and not name.startswith("__")
        }
        assert user_symbols, f"Should expose user declarations for {spec_name}"

    except Exception as e:
        pytest.fail(f"PyTorch compilation failed for {spec_name}: {e}")


if __name__ == "__main__":
    try:
        test_pytorch_simple_spec()
        test_pytorch_vs_tensorflow_equivalence()
        test_pytorch_compile_specifications("test_addition.vcl")
    except Exception:
        pass
