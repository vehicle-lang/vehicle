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


#@pytest.mark.skip()
def test_pytorch_search_single_input() -> None:
    torch, loss_pt = require_pytorch()
    spec_path = "../vehicle/tests/golden/specifications/bounded/spec.vcl"

    seed = 123
    torch.manual_seed(seed)

    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )
    networks = {"f": model}

    search_spec = loss_pt.load_search_specification(spec_path, logic=DL2DifferentiableLogic(), networks=networks)

    declarations = search_spec.declarations
    property_data = search_spec.property_data
    quantifier_data = search_spec.quantifier_data

    for property, search_counterexamples in property_data.items():
        samples = loss_pt.search(
            quantifier_data=quantifier_data[property], 
            loss_fn=declarations[property],
            num_samples=10,
            num_steps=5,
            seed=seed)

        assert(len(samples) == 10)
        
        if search_counterexamples:
            print(f"{property} COUNTER-EXAMPLES: \n")
        else:
            print(f"{property} WITNESSES: \n")

        for sample in samples:
            print(sample)
            print("\n")

            initial_loss = sample.loss_history[0]
            final_loss = sample.loss

            assert (final_loss <= initial_loss)


#@pytest.mark.skip()
def test_pytorch_search_multiple_inputs() -> None:
    torch, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_quantifier_nested.vcl"

    seed = 123
    torch.manual_seed(seed)

    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )
    networks = {"f": model}

    search_spec = loss_pt.load_search_specification(spec_path, logic=DL2DifferentiableLogic(), networks=networks)

    declarations = search_spec.declarations
    property_data = search_spec.property_data
    quantifier_data = search_spec.quantifier_data

    for property, search_counterexamples in property_data.items():
        samples = loss_pt.search(
            quantifier_data=quantifier_data[property], 
            loss_fn=declarations[property],
            num_samples=10,
            num_steps=5,
            seed=seed)

        assert(len(samples) == 10)
        
        if search_counterexamples:
            print(f"{property} COUNTER-EXAMPLES: \n")
        else:
            print(f"{property} WITNESSES: \n")

        for sample in samples:
            print(sample)
            print("\n")

            initial_loss = sample.loss_history[0]
            final_loss = sample.loss

            assert (final_loss <= initial_loss)