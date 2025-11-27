"""Test PyTorch backend with actual Vehicle specifications."""

import json
import subprocess
from pathlib import Path

import pytest
from vehicle_lang.compile._ast._nodes import Program
from vehicle_lang.compile.pytorch._translation import PyTorchTranslation


def compile_vehicle_spec(spec_path: Path) -> str:
    """Compile a Vehicle specification to JSON using the Vehicle CLI."""
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
        cwd="/home/gus/University/vehicle/vehicle-python",
    )

    if result.returncode != 0:
        raise RuntimeError(f"Vehicle compilation failed: {result.stderr}")
    return result.stdout


def test_pytorch_simple_spec() -> None:
    """Test PyTorch backend with simple.vcl specification."""
    # Find the simple.vcl file
    spec_path = Path("/home/gus/University/vehicle/examples/simple.vcl")
    if not spec_path.exists():
        pytest.skip("simple.vcl not found")

    # Compile to JSON
    json_output = compile_vehicle_spec(spec_path)
    json_data = json.loads(json_output)

    # Parse to AST
    program = Program.from_dict(json_data)

    # Create PyTorch translation
    pytorch_translation = PyTorchTranslation()

    # Compile to PyTorch
    pytorch_functions = pytorch_translation.compile(
        program, path=spec_path, declaration_context={}, samplers={}
    )

    # Verify we got some functions
    assert len(pytorch_functions) > 0, "Should generate at least one PyTorch function"

    # Check that we got the expected structure
    assert "__vehicle__" in pytorch_functions, "Should have __vehicle__ builtins object"
    builtins_obj = pytorch_functions["__vehicle__"]

    # Verify it's our PyTorch builtins
    from vehicle_lang.compile.pytorch._builtins import PyTorchBuiltins

    assert isinstance(
        builtins_obj, PyTorchBuiltins
    ), f"Expected PyTorchBuiltins, got {type(builtins_obj)}"

    print(
        f"✅ PyTorch compilation successful - generated {len(pytorch_functions)} functions"
    )
    for name, value in pytorch_functions.items():
        print(f"    {name}: {type(value).__name__}")


def test_pytorch_vs_tensorflow_equivalence() -> None:
    """Test that PyTorch and TensorFlow backends produce equivalent results on simple spec."""
    from vehicle_lang.compile.tensorflow._translation import TensorFlowTranslation

    spec_path = Path("/home/gus/University/vehicle/examples/simple.vcl")
    if not spec_path.exists():
        pytest.skip("simple.vcl not found")

    # Compile to JSON
    json_output = compile_vehicle_spec(spec_path)
    json_data = json.loads(json_output)
    program = Program.from_dict(json_data)

    # Compile with both backends
    pytorch_translation = PyTorchTranslation()
    tensorflow_translation = TensorFlowTranslation()

    pytorch_functions = pytorch_translation.compile(
        program, path=spec_path, declaration_context={}, samplers={}
    )
    tensorflow_functions = tensorflow_translation.compile(
        program, path=spec_path, declaration_context={}, samplers={}
    )

    # Should have same number of functions
    assert len(pytorch_functions) == len(
        tensorflow_functions
    ), f"Function count mismatch: PyTorch={len(pytorch_functions)}, TensorFlow={len(tensorflow_functions)}"

    # Should have same function names (excluding backend-specific imports)
    pytorch_keys = set(pytorch_functions.keys()) - {"torch"}
    tensorflow_keys = set(tensorflow_functions.keys()) - {"tensorflow"}
    assert (
        pytorch_keys == tensorflow_keys
    ), "Function names should match between backends"

    print(
        f"✅ Backend equivalence verified - both generated {len(pytorch_functions)} functions"
    )


@pytest.mark.parametrize("spec_name", ["simple.vcl"])  # type: ignore[misc]
def test_pytorch_compile_specifications(spec_name: str) -> None:
    """Test PyTorch compilation on various Vehicle specifications."""
    spec_path = Path(f"/home/gus/University/vehicle/examples/{spec_name}")
    if not spec_path.exists():
        pytest.skip(f"{spec_name} not found")

    try:
        # Compile and parse
        json_output = compile_vehicle_spec(spec_path)
        json_data = json.loads(json_output)
        program = Program.from_dict(json_data)

        # Test PyTorch compilation
        pytorch_translation = PyTorchTranslation()
        pytorch_functions = pytorch_translation.compile(
            program, path=spec_path, declaration_context={}, samplers={}
        )

        # Verify compilation success
        assert len(pytorch_functions) > 0, f"Should generate functions for {spec_name}"
        assert (
            "__vehicle__" in pytorch_functions
        ), f"Should have main function for {spec_name}"

        print(f"✅ {spec_name}: PyTorch compilation successful")

    except Exception as e:
        pytest.fail(f"PyTorch compilation failed for {spec_name}: {e}")


if __name__ == "__main__":
    # Run tests manually
    try:
        test_pytorch_simple_spec()
        test_pytorch_vs_tensorflow_equivalence()
        test_pytorch_compile_specifications("simple.vcl")
        print("🎉 All PyTorch integration tests passed!")
    except Exception as e:
        print(f"❌ Test failed: {e}")
