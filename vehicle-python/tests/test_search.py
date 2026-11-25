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


def unbatched(model: Any) -> Any:
    """Wraps a Keras model so that it can be applied to a single unbatched input."""
    return lambda x: model(x[None, :])[0]


def test_pytorch_search_bounded() -> None:
    torch, loss_pt = require_pytorch()

    spec_path = Path(__file__).parent / "data" / "test_bounded.vcl"
    declarations = ["network", "bounded"]

    # A network that calculates y = 2x + bias
    model = torch.nn.Linear(1, 1, bias=False)
    with torch.no_grad():
        model.weight.fill_(2.0)

    networks = {"network": model}

    search_results = loss_pt.search(
        spec_path,
        logic=VehicleDifferentiableLogic(),
        declarations=declarations,
        networks=networks,
        num_searches=5,
    )

    for property, results in search_results.items():
        print(f"Property: {property} \n")

        for samples in results:
            if len(samples) > 0:
                # If a counter-example has been found, it should consist
                # of only one sample
                assert len(samples) == 1
                x = samples[0]["x"].unsqueeze(0)

                output = model(x)

                print(f"Counter-example: {samples[0]}")
                print(f"Output: {output.item()} \n")

                # Check that the counter-example actually violates the property
                assert 0 < x < 1
                assert not (0 < output < 1)


def test_pytorch_search_andGate() -> None:
    torch, loss_pt = require_pytorch()

    spec_path = GOLDEN_SPECS_BASE / "andGate" / "spec.vcl"
    declarations = ["andGate", "andGateCorrect"]

    # A network which takes a tensor [x, y] and calculates x - y
    model = torch.nn.Linear(2, 1, bias=False)
    with torch.no_grad():
        model.weight[:] = torch.tensor([[1.0, -1.0]])

    networks = {"andGate": model}

    search_results = loss_pt.search(
        spec_path,
        logic=VehicleDifferentiableLogic(),
        declarations=declarations,
        networks=networks,
        num_searches=5,
    )

    for property, results in search_results.items():
        print(f"Property: {property} \n")

        for samples in results:
            if len(samples) > 0:
                # If a counter-example has been found, it should consist
                # of only one sample
                assert len(samples) == 1
                x1 = samples[0]["x1"]
                x2 = samples[0]["x2"]

                inputs = torch.stack([x1, x2])
                output = model(inputs)

                print(f"Counter-example: {samples[0]}")
                print(f"Output: {output.item()} \n")

                # Check that the counter-example actually violates the property
                assert (0 <= x1 <= 1) and (0 <= x2 <= 2)
                assert (
                    (x1 >= 0.5 and x2 >= 0.5 and not output >= 0.5)
                    or (x1 >= 0.5 and x2 <= 0.5 and not output <= 0.5)
                    or (x1 <= 0.5 and x2 >= 0.5 and not output <= 0.5)
                    or (x1 <= 0.5 and x2 <= 0.5 and not output <= 0.5)
                )


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
        logic=VehicleDifferentiableLogic(),
        declarations=declarations,
        networks=networks,
        num_searches=5,
    )

    for property, results in search_results.items():
        print(f"Property: {property} \n")

        for samples in results:
            if len(samples) > 0:
                # If a counter-example has been found, it should consist
                # of only one sample
                assert len(samples) == 1
                x = samples[0]["x"].unsqueeze(0)

                output = model(x)

                print(f"Counter-example: {samples[0]}")
                print(f"Output: {output.item()} \n")

                # Check that the counter-example actually violates the property
                assert 0 < x < 1
                assert not (x <= output)


def test_tensorflow_search_bounded() -> None:
    tf, loss_tf = require_tensorflow()

    spec_path = Path(__file__).parent / "data" / "test_bounded.vcl"
    declarations = ["network", "bounded"]

    # A network that calculates y = 2x
    model = tf.keras.Sequential(
        [tf.keras.layers.Input(shape=(1,)), tf.keras.layers.Dense(1, use_bias=False)]
    )
    model.layers[0].set_weights([tf.constant([[2.0]]).numpy()])

    search_results = loss_tf.search(
        spec_path,
        logic=VehicleDifferentiableLogic(),
        declarations=declarations,
        networks={"network": unbatched(model)},
        num_searches=5,
    )

    assert set(search_results) == {"bounded"}
    for results in search_results.values():
        assert len(results) == 5
        for samples in results:
            if len(samples) > 0:
                assert len(samples) == 1
                x = float(samples[0]["x"])
                output = 2.0 * x

                # Check that the counter-example actually violates the property
                assert 0 < x < 1
                assert not (0 < output < 1)


def test_tensorflow_search_andGate() -> None:
    tf, loss_tf = require_tensorflow()

    spec_path = GOLDEN_SPECS_BASE / "andGate" / "spec.vcl"
    declarations = ["andGate", "andGateCorrect"]

    # A network which takes a tensor [x, y] and calculates x - y
    model = tf.keras.Sequential(
        [tf.keras.layers.Input(shape=(2,)), tf.keras.layers.Dense(1, use_bias=False)]
    )
    model.layers[0].set_weights([tf.constant([[1.0], [-1.0]]).numpy()])

    search_results = loss_tf.search(
        spec_path,
        logic=VehicleDifferentiableLogic(),
        declarations=declarations,
        networks={"andGate": unbatched(model)},
        num_searches=5,
    )

    assert set(search_results) == {"andGateCorrect"}
    for results in search_results.values():
        assert len(results) == 5
        for samples in results:
            if len(samples) > 0:
                assert len(samples) == 1
                x1 = float(samples[0]["x1"])
                x2 = float(samples[0]["x2"])
                output = x1 - x2

                # Check that the counter-example actually violates the property
                assert (0 <= x1 <= 1) and (0 <= x2 <= 1)
                assert (
                    (x1 >= 0.5 and x2 >= 0.5 and not output >= 0.5)
                    or (x1 >= 0.5 and x2 <= 0.5 and not output <= 0.5)
                    or (x1 <= 0.5 and x2 >= 0.5 and not output <= 0.5)
                    or (x1 <= 0.5 and x2 <= 0.5 and not output <= 0.5)
                )


def test_tensorflow_search_increasing() -> None:
    tf, loss_tf = require_tensorflow()

    spec_path = Path(__file__).parent / "data" / "test_increasing.vcl"
    declarations = ["f", "increasing"]

    # A network that calculates y = |x - 0.5|
    model = tf.keras.Sequential(
        [
            tf.keras.layers.Input(shape=(1,)),
            tf.keras.layers.Dense(2, activation="relu"),
            tf.keras.layers.Dense(1, use_bias=False),
        ]
    )
    model.layers[0].set_weights(
        [tf.constant([[1.0, -1.0]]).numpy(), tf.constant([-0.5, 0.5]).numpy()]
    )
    model.layers[1].set_weights([tf.constant([[1.0], [1.0]]).numpy()])

    search_results = loss_tf.search(
        spec_path,
        logic=VehicleDifferentiableLogic(),
        declarations=declarations,
        networks={"f": unbatched(model)},
        num_searches=5,
    )

    assert set(search_results) == {"increasing"}
    for results in search_results.values():
        assert len(results) == 5
        for samples in results:
            if len(samples) > 0:
                assert len(samples) == 1
                x = float(samples[0]["x"])
                output = abs(x - 0.5)

                # Check that the counter-example actually violates the property
                assert 0 < x < 1
                assert not (x <= output)
