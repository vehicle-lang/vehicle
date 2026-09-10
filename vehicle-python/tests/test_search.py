"""Test the generation of witnesses and adversarial examples for properties using gradient-based search."""

from pathlib import Path
from typing import Any, Tuple

import pytest

from vehicle_lang.typing import DL2DifferentiableLogic, VehicleDifferentiableLogic

from .config import HASKELL_GOLDEN_TESTS_PATH

GOLDEN_SPECS_BASE = HASKELL_GOLDEN_TESTS_PATH / "specifications"


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


def test_pytorch_search_bounded() -> None:
    torch, loss_pt = require_pytorch()

    spec_path = Path(__file__).parent / "data" / "test_bounded.vcl"
    declarations = ["network", "bounded"]

    # A network that calculates y = 2x + bias
    model = torch.nn.Linear(1, 1)
    with torch.no_grad():
        model.weight.fill_(2.0)

    networks = {"network": model}

    search_results = loss_pt.search(
        spec_path,
        logic=DL2DifferentiableLogic(),
        declarations=declarations,
        networks=networks,
        num_searches=5,
        num_steps=10,
    )

    for property, results in search_results.items():
        print(f"Property: {property} \n")

        for boolean_result, samples in results:
            print(f"Boolean result: {boolean_result}")

            if boolean_result is False:
                # If this property evaluates to False, this means we have found
                # exactly one counter-example
                assert len(samples) == 1
                x = samples[0].inputs["x"].unsqueeze(0)

                output = model(x)

                print(f"Sample: {samples[0]}")
                print(f"Output: {output.item()} \n")

                # Check that the counter-example actually violates the property
                assert 0 < x < 1
                assert not (0 < output < 1)
            else:
                # If this property evaluates to True, this means we did not
                # manage to find any counter-examples
                assert len(samples) == 0


def test_pytorch_search_andGate() -> None:
    torch, loss_pt = require_pytorch()

    spec_path = GOLDEN_SPECS_BASE / "andGate" / "spec.vcl"
    declarations = ["andGate", "andGateCorrect"]

    # A network which takes a tensor [x, y] and calculates x - y + bias
    model = torch.nn.Linear(2, 1)
    with torch.no_grad():
        model.weight[:] = torch.tensor([[1.0, -1.0]])

    networks = {"andGate": model}

    search_results = loss_pt.search(
        spec_path,
        logic=DL2DifferentiableLogic(),
        declarations=declarations,
        networks=networks,
        num_searches=5,
        num_steps=10,
    )

    for property, results in search_results.items():
        print(f"Property: {property} \n")

        for boolean_result, samples in results:
            print(f"Boolean result: {boolean_result}")

            if boolean_result is False:
                # If this property evaluates to False, this means we have found
                # exactly one counter-example
                assert len(samples) == 1
                x1 = samples[0].inputs["x1"]
                x2 = samples[0].inputs["x2"]

                inputs = torch.stack([x1, x2])
                output = model(inputs)

                print(f"Sample: {samples[0]}")
                print(f"Output: {output.item()} \n")

                # Check that the counter-example actually violates the property
                assert (0 <= x1 <= 1) and (0 <= x2 <= 2)
                assert (
                    (x1 >= 0.5 and x2 >= 0.5 and not output >= 0.5)
                    or (x1 >= 0.5 and x2 <= 0.5 and not output <= 0.5)
                    or (x1 <= 0.5 and x2 >= 0.5 and not output <= 0.5)
                    or (x1 <= 0.5 and x2 <= 0.5 and not output <= 0.5)
                )
            else:
                # If this property evaluates to True, this means we did not
                # manage to find any counter-examples
                assert len(samples) == 0


def test_pytorch_search_increasing() -> None:
    torch, loss_pt = require_pytorch()

    spec_path = Path(__file__).parent / "data" / "test_increasing.vcl"
    declarations = ["f", "increasing"]

    # A network that calculates y = |x - 0.5|
    model = torch.nn.Sequential(
        torch.nn.Linear(1, 2), torch.nn.ReLU(), torch.nn.Linear(2, 1, bias=False)
    )

    with torch.no_grad():
        model[0].weight[:] = torch.tensor([[1.0], [-1.0]])
        model[0].bias[:] = torch.tensor([-0.5, 0.5])
        model[2].weight[:] = torch.tensor([1.0, 1.0])

    networks = {"f": model}

    search_results = loss_pt.search(
        spec_path,
        logic=DL2DifferentiableLogic(),
        declarations=declarations,
        networks=networks,
        num_steps=10,
    )

    for property, results in search_results.items():
        print(f"Property: {property} \n")

        for boolean_result, samples in results:
            print(f"Boolean result: {boolean_result}")

            if boolean_result is False:
                # If this property evaluates to False, this means we have found
                # exactly one counter-example
                assert len(samples) == 1
                x = samples[0].inputs["x"].unsqueeze(0)

                output = model(x)

                print(f"Sample: {samples[0]}")
                print(f"Output: {output.item()} \n")

                # Check that the counter-example actually violates the property
                assert 0 < x < 1
                assert not (x <= output)
            else:
                # If this property evaluates to True, this means we did not
                # manage to find any counter-examples
                assert len(samples) == 0
