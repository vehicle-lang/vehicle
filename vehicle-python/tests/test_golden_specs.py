"""Tests for golden spec files that should load and compile successfully."""

from pathlib import Path

import pytest
import vehicle_lang as vcl

GOLDEN_SPECS_BASE = (
    Path(__file__).parent.parent.parent / "vehicle" / "tests" / "golden" / "compile"
)

GOLDEN_SPEC_FILES = [
    GOLDEN_SPECS_BASE / "reachability" / "spec.vcl",
    GOLDEN_SPECS_BASE / "monotonicity" / "spec.vcl",
    GOLDEN_SPECS_BASE / "windController" / "spec.vcl",
]


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[misc]
def test_golden_spec_load(spec_path: Path) -> None:
    """Test that golden specs can be loaded into AST."""
    vcl.compile._ast.load(spec_path, target=vcl.DifferentiableLogic.DL2)


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[misc]
def test_golden_spec_tensorflow_compile(spec_path: Path) -> None:
    """Test that golden specs compile to TensorFlow."""
    output = vcl.load_specification(
        spec_path,
        backend=vcl.LossBackend.TensorFlow,
        logic=vcl.DifferentiableLogic.DL2,
        samplers={},
    )

    assert isinstance(output, dict)
    # Should have at least the spec's main property/function
    user_declarations = [k for k in output.keys() if not k.startswith("__")]
    assert len(user_declarations) > 0


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[misc]
def test_golden_spec_pytorch_compile(spec_path: Path) -> None:
    """Test that golden specs compile to PyTorch."""
    output = vcl.load_specification(
        spec_path,
        backend=vcl.LossBackend.PyTorch,
        logic=vcl.DifferentiableLogic.DL2,
        samplers={},
    )

    assert isinstance(output, dict)
    # Should have at least the spec's main property/function
    user_declarations = [k for k in output.keys() if not k.startswith("__")]
    assert len(user_declarations) > 0
