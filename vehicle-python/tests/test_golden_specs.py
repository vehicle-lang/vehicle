"""Tests for golden spec files that should load and compile successfully."""

from pathlib import Path

import pytest

import vehicle_lang as vcl
from vehicle_lang.loss import _ast as loss_ast

GOLDEN_SPECS_BASE = (
    Path(__file__).parent.parent.parent
    / "vehicle"
    / "tests"
    / "golden"
    / "specifications"
)

GOLDEN_SPEC_FILES = [
    GOLDEN_SPECS_BASE / "reachability" / "spec.vcl",
    GOLDEN_SPECS_BASE / "monotonicity" / "spec.vcl",
    GOLDEN_SPECS_BASE / "windController" / "spec.vcl",
]


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[untyped-decorator]
def test_golden_spec_load(spec_path: Path) -> None:
    """Test that golden specs can be loaded into AST."""
    loss_ast.load(spec_path, target=vcl.DifferentiableLogic.DL2)


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[untyped-decorator]
def test_golden_spec_list_decode(spec_path: Path) -> None:
    """Test that golden specs can be listed and decoded into structured entities."""
    entities = vcl.list(spec_path)
    assert isinstance(entities, list)


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[untyped-decorator]
def test_golden_spec_tensorflow_compile(spec_path: Path) -> None:
    """Test that golden specs compile to TensorFlow."""
    loss_tf = pytest.importorskip(
        "vehicle_lang.loss.tensorflow",
        reason="vehicle_lang[tensorflow] extra is not installed",
    )
    output = loss_tf.load_specification(
        spec_path,
        logic=vcl.DifferentiableLogic.DL2,
        samplers={},
    )

    assert isinstance(output, dict)
    # Should have at least the spec's main property/function
    user_declarations = [k for k in output.keys() if not k.startswith("__")]
    assert len(user_declarations) > 0


@pytest.mark.parametrize("spec_path", GOLDEN_SPEC_FILES)  # type: ignore[untyped-decorator]
def test_golden_spec_pytorch_compile(spec_path: Path) -> None:
    """Test that golden specs compile to PyTorch."""
    loss_pt = pytest.importorskip(
        "vehicle_lang.loss.pytorch",
        reason="vehicle_lang[pytorch] extra is not installed",
    )
    output = loss_pt.load_specification(
        spec_path,
        logic=vcl.DifferentiableLogic.DL2,
        samplers={},
    )

    assert isinstance(output, dict)
    # Should have at least the spec's main property/function
    user_declarations = [k for k in output.keys() if not k.startswith("__")]
    assert len(user_declarations) > 0
