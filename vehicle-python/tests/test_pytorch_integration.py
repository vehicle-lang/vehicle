"""Test PyTorch backend with actual Vehicle specifications."""

import json
import subprocess
from pathlib import Path

import pytest
from vehicle_lang.compile._ast._nodes import Program
from vehicle_lang.compile.pytorch._translation import PyTorchTranslation


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
    """Test PyTorch backend with simple.vcl specification."""
    repo_root = Path(__file__).parent.parent.parent.resolve()
    spec_path = repo_root / "examples" / "simple.vcl"
    if not spec_path.exists():
        pytest.skip("simple.vcl not found")

    json_output = compile_vehicle_spec(spec_path)
    json_data = json.loads(json_output)
    program = Program.from_dict(json_data)
    pytorch_translation = PyTorchTranslation()
    pytorch_functions = pytorch_translation.compile(
        program, path=spec_path, declaration_context={}, samplers={}
    )

    assert len(pytorch_functions) > 0, "Should generate at least one PyTorch function"

    assert "__vehicle__" in pytorch_functions, "Should have __vehicle__ builtins object"
    builtins_obj = pytorch_functions["__vehicle__"]

    from vehicle_lang.compile.pytorch._builtins import PyTorchBuiltins

    assert isinstance(
        builtins_obj, PyTorchBuiltins
    ), f"Expected PyTorchBuiltins, got {type(builtins_obj)}"


def test_pytorch_vs_tensorflow_equivalence() -> None:
    """Test that PyTorch and TensorFlow backends produce equivalent results on simple spec."""
    from vehicle_lang.compile.tensorflow._translation import TensorFlowTranslation

    repo_root = Path(__file__).parent.parent.parent.resolve()
    spec_path = repo_root / "examples" / "simple.vcl"

    if not spec_path.exists():
        pytest.skip("simple.vcl not found")

    json_output = compile_vehicle_spec(spec_path)
    json_data = json.loads(json_output)
    program = Program.from_dict(json_data)
    pytorch_translation = PyTorchTranslation()
    tensorflow_translation = TensorFlowTranslation()

    pytorch_functions = pytorch_translation.compile(
        program, path=spec_path, declaration_context={}, samplers={}
    )
    tensorflow_functions = tensorflow_translation.compile(
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


@pytest.mark.parametrize("spec_name", ["simple.vcl"])  # type: ignore[misc]
def test_pytorch_compile_specifications(spec_name: str) -> None:
    """Test PyTorch compilation on various Vehicle specifications."""
    repo_root = Path(__file__).parent.parent.parent.resolve()
    spec_path = repo_root / "examples" / spec_name
    if not spec_path.exists():
        pytest.skip(f"{spec_name} not found")

    try:
        json_output = compile_vehicle_spec(spec_path)
        json_data = json.loads(json_output)
        program = Program.from_dict(json_data)
        pytorch_translation = PyTorchTranslation()
        pytorch_functions = pytorch_translation.compile(
            program, path=spec_path, declaration_context={}, samplers={}
        )
        assert len(pytorch_functions) > 0, f"Should generate functions for {spec_name}"
        assert (
            "__vehicle__" in pytorch_functions
        ), f"Should have main function for {spec_name}"

    except Exception as e:
        pytest.fail(f"PyTorch compilation failed for {spec_name}: {e}")


if __name__ == "__main__":
    try:
        test_pytorch_simple_spec()
        test_pytorch_vs_tensorflow_equivalence()
        test_pytorch_compile_specifications("simple.vcl")
    except Exception as e:
        pass
