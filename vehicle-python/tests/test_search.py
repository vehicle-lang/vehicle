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


def test_pytorch_search() -> None:
    torch, loss_pt = require_pytorch()

    spec_path = GOLDEN_SPECS_BASE / "bounded" / "spec.vcl"
    declarations = ["f", "bounded"]

    # Create a random network (in future, tailor it to the spec)
    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )
    networks = {"f": model}

    counterexamples = loss_pt.search(
        spec_path,
        logic=DL2DifferentiableLogic(),
        declarations=declarations,
        networks=networks,
        num_steps=10,
    )

    print(counterexamples)

    assert False


'''
@pytest.mark.skip()
def test_pytorch_search_single_input() -> None:
    """Test gradient-based search for properties with a single input."""
    torch, loss_pt = require_pytorch()
    spec_path = GOLDEN_SPECS_BASE / "bounded" / "spec.vcl"

    # Create a random network (in future, tailor it to the spec)
    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )
    networks = {"f": model}

    search_results = loss_pt.search(
        spec_path, logic=DL2DifferentiableLogic(), networks=networks, num_samples=10
    )

    # Check that the single property in the specification is searched
    assert len(search_results) == 1

    result = search_results[0]

    assert result.property == "bounded"
    # Check that the search produced adversarial examples, not witnesses
    # to the property as it contains only universal quantifiers
    assert len(result.witnesses) == 0
    assert len(result.adversarial_examples) == 10

    print(f"{result.property} ADVERSARIAL EXAMPLES \n")

    for adv_example in result.adversarial_examples:
        print(adv_example)
        print("\n")

        # Check that each adversarial example has 1 input
        assert len(adv_example.inputs) == 1

        final_loss = adv_example.loss
        initial_loss = adv_example.loss_history[0]

        # Check that the search minimised the loss
        assert final_loss <= initial_loss


@pytest.mark.skip()
def test_pytorch_search_multiple_inputs() -> None:
    """Test gradient-based search for properties with multiple inputs."""
    torch, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_quantifier_nested.vcl"

    # Create a random network (in future, tailor it to the spec)
    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )
    networks = {"f": model}

    search_results = loss_pt.search(
        spec_path, logic=DL2DifferentiableLogic(), networks=networks, num_samples=10
    )

    # Check that the single property in the specification is searched
    assert len(search_results) == 1

    result = search_results[0]

    assert result.property == "equalNested"
    # Check that the search produced witnesses, not adversarial examples
    # to the property as it contains only existential quantifiers
    assert len(result.witnesses) == 10
    assert len(result.adversarial_examples) == 0

    print(f"{result.property} WITNESSES \n")

    for witness in result.witnesses:
        print(witness)
        print("\n")

        # Check that witness has 2 inputs
        assert len(witness.inputs) == 2

        final_loss = witness.loss
        initial_loss = witness.loss_history[0]

        # Check that the search minimised the loss
        assert final_loss <= initial_loss
'''
