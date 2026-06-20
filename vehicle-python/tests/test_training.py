"""Test that loss functions from load_specification are trainable with autodiff."""

from pathlib import Path
from typing import Any, Tuple

import pytest

import vehicle_lang as vcl


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


def test_constraint_only_training_tensorflow() -> None:
    """Test that training with ONLY constraint loss (no task loss) actually enforces the constraint."""
    tf, loss_tf = require_tensorflow()
    spec_path = Path(__file__).parent / "data" / "test_trainable.vcl"
    declarations = loss_tf.load_specification(
        spec_path,
        logic=vcl.VehicleDifferentiableLogic(),
    )

    constraint_loss_fn = declarations["output_bounded"]

    # Create a network that VIOLATES the constraint initially
    # Initialize with large weights so outputs are >> 5
    model = tf.keras.Sequential(
        [
            tf.keras.layers.Input(shape=(1,)),
            tf.keras.layers.Dense(8, activation="relu"),
            tf.keras.layers.Dense(1),
        ]
    )

    model.layers[0].set_weights(
        [tf.random.uniform((1, 8), 5.0, 10.0).numpy(), tf.zeros(8).numpy()]
    )
    model.layers[1].set_weights(
        [
            tf.random.uniform((8, 1), 1.0, 2.0).numpy(),
            tf.constant([10.0]).numpy(),  # Large bias ensures output >> 5
        ]
    )

    def network(x: Any) -> Any:
        return tf.reshape(model(tf.reshape(x, [1, 1])), [1])

    # Verify initial constraint violation
    initial_constraint_loss = constraint_loss_fn(network)
    print(f"\nInitial constraint loss: {initial_constraint_loss.numpy()}")

    # Sample some outputs to verify they violate constraint
    test_inputs = [0.0, 0.5, 1.0]
    initial_outputs = [
        network(tf.constant(x, dtype=tf.float32))[0].numpy() for x in test_inputs
    ]
    print(f"Initial outputs at {test_inputs}: {initial_outputs}")
    assert all(
        out > 5.0 for out in initial_outputs
    ), "Network should initially violate constraint"

    optimizer = tf.keras.optimizers.Adam(learning_rate=0.05)

    # Train ONLY on constraint loss (no task loss)
    for step in range(50):
        with tf.GradientTape() as tape:
            loss = constraint_loss_fn(network)

        grads = tape.gradient(loss, model.trainable_variables)
        optimizer.apply_gradients(zip(grads, model.trainable_variables))

        if step % 10 == 0:
            print(f"Step {step}, constraint loss: {loss.numpy()}")

    # After training, check if constraint is satisfied
    final_constraint_loss = constraint_loss_fn(network)
    final_outputs = [
        network(tf.constant(x, dtype=tf.float32))[0].numpy() for x in test_inputs
    ]
    print(f"Final constraint loss: {final_constraint_loss.numpy()}")
    print(f"Final outputs at {test_inputs}: {final_outputs}")

    # Verify constraint is now satisfied (or at least much better)
    assert (
        final_constraint_loss < initial_constraint_loss
    ), "Constraint loss should decrease"
    # Most outputs should now be <= 5 (or at least closer to 5)
    assert max(final_outputs) < max(initial_outputs), "Max output should decrease"


def test_constraint_only_training_pytorch() -> None:
    """Test that training with ONLY constraint loss (no task loss) actually enforces the constraint."""
    torch, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_trainable.vcl"
    declarations = loss_pt.load_specification(
        spec_path,
        logic=vcl.VehicleDifferentiableLogic(),
    )

    constraint_loss_fn = declarations["output_bounded"]

    # Create a network that VIOLATES the constraint initially
    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )

    # Initialize with large weights
    with torch.no_grad():
        assert isinstance(model[0], torch.nn.Linear)
        assert isinstance(model[2], torch.nn.Linear)
        model[0].weight.data.uniform_(5.0, 10.0)
        model[0].bias.data.zero_()
        model[2].weight.data.uniform_(1.0, 2.0)
        model[2].bias.data.fill_(10.0)

    def network(x: Any) -> Any:
        return model(x.reshape(1, 1)).reshape(1)

    # Verify initial constraint violation
    initial_constraint_loss = constraint_loss_fn(network)
    print(f"\nInitial constraint loss: {initial_constraint_loss.item()}")

    test_inputs = [0.0, 0.5, 1.0]
    with torch.no_grad():
        initial_outputs = [
            network(torch.tensor(x, dtype=torch.float32))[0].item() for x in test_inputs
        ]
    print(f"Initial outputs at {test_inputs}: {initial_outputs}")
    assert all(
        out > 5.0 for out in initial_outputs
    ), "Network should initially violate constraint"

    optimizer = torch.optim.Adam(model.parameters(), lr=0.05)

    # Train ONLY on constraint loss (no task loss)
    for step in range(50):
        optimizer.zero_grad()
        loss = constraint_loss_fn(network)
        loss.backward()
        optimizer.step()

        if step % 10 == 0:
            print(f"Step {step}, constraint loss: {loss.item()}")

    # After training, check if constraint is satisfied
    final_constraint_loss = constraint_loss_fn(network)
    with torch.no_grad():
        final_outputs = [
            network(torch.tensor(x, dtype=torch.float32))[0].item() for x in test_inputs
        ]
    print(f"Final constraint loss: {final_constraint_loss.item()}")
    print(f"Final outputs at {test_inputs}: {final_outputs}")

    # Verify constraint is now satisfied (or at least much better)
    assert (
        final_constraint_loss.item() < initial_constraint_loss.item()
    ), "Constraint loss should decrease"
    assert max(final_outputs) < max(initial_outputs), "Max output should decrease"


def test_tensorflow_combined_loss() -> None:
    """Test that TensorFlow can train with combined task + constraint loss."""
    tf, loss_tf = require_tensorflow()
    spec_path = Path(__file__).parent / "data" / "test_trainable.vcl"
    declarations = loss_tf.load_specification(
        spec_path,
        logic=vcl.VehicleDifferentiableLogic(),
    )

    constraint_loss_fn = declarations["output_bounded"]

    # Create a simple neural network that initially violates the constraint
    model = tf.keras.Sequential(
        [
            tf.keras.layers.Input(shape=(1,)),
            tf.keras.layers.Dense(8, activation="relu"),
            tf.keras.layers.Dense(1),
        ]
    )

    # Initialize with moderate weights so not all outputs exceed 5
    # This avoids the case where all FGSM samples find perfect violations (value=0)
    # which would cause inf loss due to 1/0 in the DL2 formula
    model.layers[0].set_weights(
        [tf.random.uniform((1, 8), 1.0, 3.0).numpy(), tf.zeros(8).numpy()]
    )
    model.layers[1].set_weights(
        [
            tf.random.uniform((8, 1), 0.5, 1.5).numpy(),
            tf.random.uniform((1,), 3.0, 4.0).numpy(),  # Bias to ensure some violations
        ]
    )

    def network(x: Any) -> Any:
        return tf.reshape(model(tf.reshape(x, [1, 1])), [1])

    # Training data: learn to approximate y = 2*x
    train_x_values = [0.0, 0.25, 0.5, 0.75, 1.0]
    train_y_values = [0.0, 0.5, 1.0, 1.5, 2.0]

    optimizer = tf.keras.optimizers.Adam(learning_rate=0.01)
    alpha = 0.5  # Balance between task and constraint

    # Compute initial losses
    with tf.GradientTape() as tape:
        task_loss_sum = tf.constant(0.0)
        for x_val, y_val in zip(train_x_values, train_y_values):
            x_input = tf.constant(x_val, dtype=tf.float32)
            pred = network(x_input)[0]
            task_loss_sum = task_loss_sum + (pred - y_val) ** 2
        task_loss = task_loss_sum / len(train_x_values)

        constraint_loss = constraint_loss_fn(network)
        combined_loss = alpha * task_loss + (1.0 - alpha) * constraint_loss

    # Check that losses are finite
    assert tf.math.is_finite(task_loss)
    assert tf.math.is_finite(constraint_loss)
    assert tf.math.is_finite(combined_loss)

    # Get gradients
    grads = tape.gradient(combined_loss, model.trainable_variables)
    assert all(grad is not None for grad in grads)
    assert all(tf.math.reduce_all(tf.math.is_finite(grad)) for grad in grads)

    initial_combined = float(combined_loss)

    # Perform training step
    optimizer.apply_gradients(zip(grads, model.trainable_variables))

    # Recompute losses after training
    task_loss_sum_after = tf.constant(0.0)
    for x_val, y_val in zip(train_x_values, train_y_values):
        x_input = tf.constant(x_val, dtype=tf.float32)
        pred = network(x_input)[0]
        task_loss_sum_after = task_loss_sum_after + (pred - y_val) ** 2
    task_loss_after = task_loss_sum_after / len(train_x_values)
    constraint_loss_after = constraint_loss_fn(network)
    combined_loss_after = (
        alpha * task_loss_after + (1.0 - alpha) * constraint_loss_after
    )

    # Verify training reduced the combined loss
    assert combined_loss_after < initial_combined


def test_pytorch_combined_loss() -> None:
    """Test that PyTorch can train with combined task + constraint loss."""
    torch, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_trainable.vcl"
    declarations = loss_pt.load_specification(
        spec_path,
        logic=vcl.VehicleDifferentiableLogic(),
    )

    constraint_loss_fn = declarations["output_bounded"]

    # Create a simple neural network that initially violates the constraint
    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )

    # Initialize with large weights to ensure initial violation
    with torch.no_grad():
        assert isinstance(model[0], torch.nn.Linear)
        assert isinstance(model[2], torch.nn.Linear)
        model[0].weight.data.uniform_(5.0, 10.0)
        model[0].bias.data.zero_()
        model[2].weight.data.uniform_(0.5, 1.5)
        model[2].bias.data.zero_()

    def network(x: Any) -> Any:
        return model(x.reshape(1, 1)).reshape(1)

    # Training data: learn to approximate y = 2*x
    train_x_values = [0.0, 0.25, 0.5, 0.75, 1.0]
    train_y_values = [0.0, 0.5, 1.0, 1.5, 2.0]

    optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
    alpha = 0.5  # Balance between task and constraint

    optimizer.zero_grad()

    # Compute initial losses
    task_loss_sum = torch.tensor(0.0)
    for x_val, y_val in zip(train_x_values, train_y_values):
        x_input = torch.tensor(x_val, dtype=torch.float32)
        pred = network(x_input)[0]
        task_loss_sum = task_loss_sum + (pred - y_val) ** 2
    task_loss = task_loss_sum / len(train_x_values)

    constraint_loss = constraint_loss_fn(network)
    combined_loss = alpha * task_loss + (1.0 - alpha) * constraint_loss

    # Check that losses are finite
    assert torch.isfinite(task_loss)
    assert torch.isfinite(constraint_loss)
    assert torch.isfinite(combined_loss)

    initial_combined = combined_loss.item()

    # Backpropagate
    combined_loss.backward()

    # Check gradients are finite
    assert all(param.grad is not None for param in model.parameters())
    assert all(
        param.grad is not None and torch.isfinite(param.grad).all()
        for param in model.parameters()
    )

    # Perform training step
    optimizer.step()

    # Recompute losses after training (without no_grad so FGSM works)
    task_loss_sum_after = torch.tensor(0.0)
    for x_val, y_val in zip(train_x_values, train_y_values):
        x_input = torch.tensor(x_val, dtype=torch.float32)
        pred = network(x_input)[0]
        task_loss_sum_after = task_loss_sum_after + (pred - y_val) ** 2
    task_loss_after = task_loss_sum_after / len(train_x_values)
    constraint_loss_after = constraint_loss_fn(network)
    combined_loss_after = (
        alpha * task_loss_after + (1.0 - alpha) * constraint_loss_after
    )

    # Verify training reduced the combined loss
    assert combined_loss_after.detach() < initial_combined


def test_pytorch_multi_step_training() -> None:
    """Test multi-step training converges with combined loss."""
    torch, loss_pt = require_pytorch()
    spec_path = Path(__file__).parent / "data" / "test_trainable.vcl"
    declarations = loss_pt.load_specification(
        spec_path,
        logic=vcl.VehicleDifferentiableLogic(),
    )

    constraint_loss_fn = declarations["output_bounded"]

    # Create a simple neural network
    model = torch.nn.Sequential(
        torch.nn.Linear(1, 8), torch.nn.ReLU(), torch.nn.Linear(8, 1)
    )

    # Initialize with large weights to ensure initial violation
    assert isinstance(model[0], torch.nn.Linear)
    assert isinstance(model[2], torch.nn.Linear)
    linear0 = model[0]
    linear2 = model[2]
    torch.nn.init.uniform_(linear0.weight, 5.0, 10.0)
    torch.nn.init.zeros_(linear0.bias)
    torch.nn.init.uniform_(linear2.weight, 0.5, 1.5)
    torch.nn.init.zeros_(linear2.bias)

    def network(x: Any) -> Any:
        return model(x.reshape(1, 1)).reshape(1)

    # Training data: learn to approximate y = 2*x
    train_x_values = [0.0, 0.25, 0.5, 0.75, 1.0]
    train_y_values = [0.0, 0.5, 1.0, 1.5, 2.0]

    optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
    alpha = 0.5  # Balance between task and constraint

    # Store initial loss (compute with gradients so FGSM works)
    task_loss_sum = torch.tensor(0.0)
    for x_val, y_val in zip(train_x_values, train_y_values):
        x_input = torch.tensor(x_val, dtype=torch.float32)
        pred = network(x_input)[0]
        task_loss_sum = task_loss_sum + (pred - y_val) ** 2
    initial_task_loss = task_loss_sum / len(train_x_values)
    initial_constraint_loss = constraint_loss_fn(network)
    initial_combined_loss = (
        alpha * initial_task_loss + (1.0 - alpha) * initial_constraint_loss
    ).detach()

    # Train for multiple steps
    num_steps = 20
    for step in range(num_steps):
        optimizer.zero_grad()

        task_loss_sum = torch.tensor(0.0)
        for x_val, y_val in zip(train_x_values, train_y_values):
            x_input = torch.tensor(x_val, dtype=torch.float32)
            pred = network(x_input)[0]
            task_loss_sum = task_loss_sum + (pred - y_val) ** 2
        task_loss = task_loss_sum / len(train_x_values)

        constraint_loss = constraint_loss_fn(network)
        combined_loss = alpha * task_loss + (1.0 - alpha) * constraint_loss

        combined_loss.backward()
        optimizer.step()

    # Check final loss is lower than initial (compute with gradients so FGSM works)
    task_loss_sum = torch.tensor(0.0)
    for x_val, y_val in zip(train_x_values, train_y_values):
        x_input = torch.tensor(x_val, dtype=torch.float32)
        pred = network(x_input)[0]
        task_loss_sum = task_loss_sum + (pred - y_val) ** 2
    final_task_loss = task_loss_sum / len(train_x_values)
    final_constraint_loss = constraint_loss_fn(network)
    final_combined_loss = (
        alpha * final_task_loss + (1.0 - alpha) * final_constraint_loss
    ).detach()

    # Verify training improved the losses
    assert final_combined_loss < initial_combined_loss
    assert final_task_loss < initial_task_loss
