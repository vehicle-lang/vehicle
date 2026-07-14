from pathlib import Path
from typing import Any, Tuple

import pytest
import vehicle_lang as vcl

from vehicle_lang._ast._nodes import SearchRatTensor
from vehicle_lang.typing import VehicleDifferentiableLogic, DL2DifferentiableLogic

def require_tensorflow() -> Tuple[Any, Any]:
    tf_module = pytest.importorskip(
        "tensorflow",
        reason="TensorFlow extra is required for TensorFlow search tests",
    )
    loss_module = pytest.importorskip(
        "vehicle_lang.loss.tensorflow",
        reason="vehicle_lang[tensorflow] extra is not installed",
    )
    return tf_module, loss_module


def require_pytorch() -> Tuple[Any, Any]:
    torch_module = pytest.importorskip(
        "torch",
        reason="PyTorch extra is required for PyTorch search tests",
    )
    loss_module = pytest.importorskip(
        "vehicle_lang.loss.pytorch",
        reason="vehicle_lang[pytorch] extra is not installed",
    )
    return torch_module, loss_module


def test_nested() -> None:
    _, loss_pt = require_pytorch()
    spec_path_nested = Path(__file__).parent / "data" / "test_quantifier_nested.vcl"

    declarations_dl2, quantifiers_dl2 = loss_pt.search(spec_path_nested, logic=DL2DifferentiableLogic())

    assert (declarations_dl2["equalNested"] is not None)
    assert (declarations_dl2["leftMoreNested"] is not None)
    assert (declarations_dl2["rightMoreNested"] is not None)

    assert (len(quantifiers_dl2.items()) == 3)

    assert (len(quantifiers_dl2["equalNested"]) == 2)
    assert (len(quantifiers_dl2["leftMoreNested"]) == 3)
    assert (len(quantifiers_dl2["rightMoreNested"]) == 3)

    assert all([isinstance(x, SearchRatTensor) for x in quantifiers_dl2["equalNested"]])
    assert all([isinstance(x, SearchRatTensor) for x in quantifiers_dl2["leftMoreNested"]])
    assert all([isinstance(x, SearchRatTensor) for x in quantifiers_dl2["rightMoreNested"]])

    declarations_vehicle, quantifiers_vehicle = loss_pt.search(spec_path_nested, logic=VehicleDifferentiableLogic())

    assert (declarations_vehicle["equalNested"] is not None)
    assert (declarations_vehicle["leftMoreNested"] is not None)
    assert (declarations_vehicle["rightMoreNested"] is not None)

    assert (len(quantifiers_vehicle.items()) == 3)

    assert (len(quantifiers_vehicle["equalNested"]) == 2)
    assert (len(quantifiers_vehicle["leftMoreNested"]) == 3)
    assert (len(quantifiers_vehicle["rightMoreNested"]) == 3)

    assert all([isinstance(x, SearchRatTensor) for x in quantifiers_vehicle["equalNested"]])
    assert all([isinstance(x, SearchRatTensor) for x in quantifiers_vehicle["leftMoreNested"]])
    assert all([isinstance(x, SearchRatTensor) for x in quantifiers_vehicle["rightMoreNested"]])


def test_andGate() -> None:
    _, loss_pt = require_pytorch()
    spec_path = "/Users/kellysnow/Documents/vehicle/vehicle/tests/golden/specifications/andGate/spec.vcl"

    declarations_dl2, quantifiers_dl2 = loss_pt.search(spec_path, logic=DL2DifferentiableLogic())

    assert (declarations_dl2["andGateCorrect"] is not None)
    assert (len(quantifiers_dl2.items()) == 1)
    assert (list(quantifiers_dl2.keys())[0] == "andGateCorrect")
    assert (len(quantifiers_dl2["andGateCorrect"]) == 2)
    assert (isinstance(quantifiers_dl2["andGateCorrect"][0], SearchRatTensor))
    assert (isinstance(quantifiers_dl2["andGateCorrect"][1], SearchRatTensor))

    declarations_vehicle, quantifiers_vehicle = loss_pt.search(spec_path, logic=VehicleDifferentiableLogic())

    assert (declarations_vehicle["andGateCorrect"] is not None)
    assert (len(quantifiers_vehicle.items()) == 1)
    assert (list(quantifiers_vehicle.keys())[0] == "andGateCorrect")
    assert (len(quantifiers_vehicle["andGateCorrect"]) == 2)
    assert (isinstance(quantifiers_vehicle["andGateCorrect"][0], SearchRatTensor))
    assert (isinstance(quantifiers_vehicle["andGateCorrect"][1], SearchRatTensor))


def test_bounded() -> None:
    _, loss_pt = require_pytorch()
    spec_path = "/Users/kellysnow/Documents/vehicle/vehicle/tests/golden/specifications/bounded/spec.vcl"

    declarations_dl2, quantifiers_dl2 = loss_pt.search(spec_path, logic=DL2DifferentiableLogic())

    assert (declarations_dl2["bounded"] is not None)
    assert (len(quantifiers_dl2.items()) == 1)
    assert (list(quantifiers_dl2.keys())[0] == "bounded")
    assert (len(quantifiers_dl2["bounded"]) == 1)
    assert (isinstance(quantifiers_dl2["bounded"][0], SearchRatTensor))

    declarations_vehicle, quantifiers_vehicle = loss_pt.search(spec_path, logic=VehicleDifferentiableLogic())

    assert (declarations_vehicle["bounded"] is not None)
    assert (len(quantifiers_vehicle.items()) == 1)
    assert (list(quantifiers_vehicle.keys())[0] == "bounded")
    assert (len(quantifiers_vehicle["bounded"]) == 1)
    assert (isinstance(quantifiers_vehicle["bounded"][0], SearchRatTensor))


def test_monotonicity() -> None:
    _, loss_pt = require_pytorch()
    spec_path = "/Users/kellysnow/Documents/vehicle/vehicle/tests/golden/specifications/monotonicity/spec.vcl"

    declarations_dl2, quantifiers_dl2 = loss_pt.search(spec_path, logic=DL2DifferentiableLogic())

    assert (declarations_dl2["monotonic"] is not None)
    assert (len(quantifiers_dl2.items()) == 1)
    assert (list(quantifiers_dl2.keys())[0] == "monotonic")
    assert (len(quantifiers_dl2["monotonic"]) == 2)
    assert (isinstance(quantifiers_dl2["monotonic"][0], SearchRatTensor))
    assert (isinstance(quantifiers_dl2["monotonic"][1], SearchRatTensor))

    declarations_vehicle, quantifiers_vehicle = loss_pt.search(spec_path, logic=VehicleDifferentiableLogic())

    assert (declarations_vehicle["monotonic"] is not None)
    assert (len(quantifiers_vehicle.items()) == 1)
    assert (list(quantifiers_vehicle.keys())[0] == "monotonic")
    assert (len(quantifiers_vehicle["monotonic"]) == 2)
    assert (isinstance(quantifiers_vehicle["monotonic"][0], SearchRatTensor))
    assert (isinstance(quantifiers_vehicle["monotonic"][1], SearchRatTensor))


def test_reachability() -> None:
    _, loss_pt = require_pytorch()
    spec_path = "/Users/kellysnow/Documents/vehicle/vehicle/tests/golden/specifications/reachability/spec.vcl"

    declarations_dl2, quantifiers_dl2 = loss_pt.search(spec_path, logic=DL2DifferentiableLogic())

    assert (declarations_dl2["reachable"] is not None)
    assert (len(quantifiers_dl2.items()) == 1)
    assert (list(quantifiers_dl2.keys())[0] == "reachable")
    assert (len(quantifiers_dl2["reachable"]) == 1)
    assert (isinstance(quantifiers_dl2["reachable"][0], SearchRatTensor))

    declarations_vehicle, quantifiers_vehicle = loss_pt.search(spec_path, logic=VehicleDifferentiableLogic())

    assert (declarations_vehicle["reachable"] is not None)
    assert (len(quantifiers_vehicle.items()) == 1)
    assert (list(quantifiers_vehicle.keys())[0] == "reachable")
    assert (len(quantifiers_vehicle["reachable"]) == 1)
    assert (isinstance(quantifiers_vehicle["reachable"][0], SearchRatTensor))


def test_windController() -> None:
    _, loss_pt = require_pytorch()
    spec_path = "/Users/kellysnow/Documents/vehicle/vehicle/tests/golden/specifications/windController/spec.vcl"

    declarations_dl2, quantifiers_dl2 = loss_pt.search(spec_path, logic=DL2DifferentiableLogic())

    assert (declarations_dl2["safe"] is not None)
    assert (len(quantifiers_dl2.items()) == 1)
    assert (list(quantifiers_dl2.keys())[0] == "safe")
    assert (len(quantifiers_dl2["safe"]) == 1)
    assert (isinstance(quantifiers_dl2["safe"][0], SearchRatTensor))

    declarations_vehicle, quantifiers_vehicle = loss_pt.search(spec_path, logic=VehicleDifferentiableLogic())

    assert (declarations_vehicle["safe"] is not None)
    assert (len(quantifiers_vehicle.items()) == 1)
    assert (list(quantifiers_vehicle.keys())[0] == "safe")
    assert (len(quantifiers_vehicle["safe"]) == 1)
    assert (isinstance(quantifiers_vehicle["safe"][0], SearchRatTensor)) 