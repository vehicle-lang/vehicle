from pathlib import Path
from typing import Any, Tuple

import pytest
import vehicle_lang as vcl

from vehicle_lang._ast._nodes import SearchRatTensor

def require_tensorflow() -> Tuple[Any, Any]:
    tf_module = pytest.importorskip(
        "tensorflow",
        reason="TensorFlow extra is required for TensorFlow training tests",
    )
    loss_module = pytest.importorskip(
        "vehicle_lang.loss.tensorflow",
        reason="vehicle_lang[tensorflow] extra is not installed",
    )
    return tf_module, loss_module


def require_pytorch() -> Tuple[Any, Any]:
    torch_module = pytest.importorskip(
        "torch",
        reason="PyTorch extra is required for PyTorch training tests",
    )
    loss_module = pytest.importorskip(
        "vehicle_lang.loss.pytorch",
        reason="vehicle_lang[pytorch] extra is not installed",
    )
    return torch_module, loss_module


def test_lift_and_get_quantifiers() -> None:
    _, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_quantifier_any.vcl"
    
    _, quantifiers = loss_pt.load_specification(
        spec_path,
        mode="CounterExample",
        logic=vcl.DifferentiableLogic.DL2,
	)
    assert (len(quantifiers.items()) == 1)
    assert (list(quantifiers.keys())[0] == "equalNested")
    assert (len(quantifiers["equalNested"]) == 2)
    assert (isinstance(quantifiers["equalNested"][0], SearchRatTensor))
    assert (isinstance(quantifiers["equalNested"][1], SearchRatTensor))