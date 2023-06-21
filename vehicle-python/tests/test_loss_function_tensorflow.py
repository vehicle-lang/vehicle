import random
from pathlib import Path
from typing import Any, Iterator

from vehicle_lang.loss_function import compile


def test_loss_function_tensorflow_bounded() -> None:
    try:
        import numpy as np
        import tensorflow as tf

        # Prepare a simple network
        network = tf.keras.Sequential(
            [
                tf.keras.layers.Input(shape=(1,)),
                tf.keras.layers.Dense(units=1),
            ]
        )

        def _apply_network(input: Any) -> Any:
            return network(tf.expand_dims(input, axis=0), training=True)[0]

        # Load and compile Vehicle specification
        specification_filename = "test_bounded.vcl"
        specification_path = Path(__file__).parent / "data" / specification_filename

        def _sampler_for_x(**ctx: Any) -> Iterator[float]:
            yield random.uniform(0.5, 0.5)

        _bounded_loss = compile.to_tensorflow(
            specification_path,
            "bounded",
            declaration_context={"f": _apply_network, "sampler_for_x": _sampler_for_x},
        )

        # Prepare training data
        batch_size = 1

        X_train = np.array([[0.0], [0.2], [0.4], [0.6], [0.8]])
        X_test = np.array([[0.1], [0.3], [0.5], [0.7], [0.9]])
        y_train = np.array([0, 0, 0, 1, 1])
        y_test = np.array([0, 0, 1, 1, 1])

        train_dataset = tf.data.Dataset.from_tensor_slices((X_train, y_train))
        test_dataset = tf.data.Dataset.from_tensor_slices((X_test, y_test))

        train_dataset = train_dataset.shuffle(buffer_size=1024).batch(batch_size)
        test_dataset = test_dataset.batch(batch_size)

        # Train the network
        num_epochs = 4

        optimizer = tf.keras.optimizers.legacy.Adam()
        ce_batch_loss = tf.keras.losses.BinaryCrossentropy()

        train_acc_metric = tf.keras.metrics.BinaryCrossentropy()
        test_acc_metric = tf.keras.metrics.BinaryCrossentropy()
        train_loss_metric = tf.keras.metrics.BinaryCrossentropy()
        test_loss_metric = tf.keras.metrics.BinaryCrossentropy()

        ce_loss_weight = 0
        bounded_weight = 1

        for epoch in range(num_epochs):
            print(f"\nEpoch {epoch + 1}")

            # Iterate over the batches of the dataset.
            for x_batch_train, y_batch_train in train_dataset:
                # Open a GradientTape to record the operations run during the forward pass, which enables auto-differentiation.
                with tf.GradientTape() as tape:
                    outputs = network(
                        x_batch_train, training=True
                    )  # Outputs for this minibatch
                    ce_loss_value = ce_batch_loss(y_batch_train, outputs)
                    bounded_value = _bounded_loss()
                    weighted_loss = (
                        ce_loss_value * ce_loss_weight + bounded_value * bounded_weight
                    )
                # Use the gradient tape to automatically retrieve the gradients of the trainable variables with respect to the loss.
                grads = tape.gradient(weighted_loss, network.trainable_weights)
                # Run one step of gradient descent by updating the value of the variables to minimize the loss.
                optimizer.apply_gradients(zip(grads, network.trainable_weights))

            # Run a training loop at the end of each epoch.
            for x_batch_train, y_batch_train in train_dataset:
                train_outputs = network(x_batch_train, training=False)
                train_acc_metric.update_state(y_batch_train, train_outputs)
                train_loss_metric.update_state(y_batch_train, train_outputs)

            # Run a testing loop at the end of each epoch.
            for x_batch_test, y_batch_test in test_dataset:
                test_outputs = network(x_batch_test, training=False)
                test_acc_metric.update_state(y_batch_test, test_outputs)
                test_loss_metric.update_state(y_batch_test, test_outputs)

            train_acc = train_acc_metric.result()
            test_acc = test_acc_metric.result()
            train_loss = train_loss_metric.result()
            test_loss = test_loss_metric.result()

            train_acc_metric.reset_states()
            test_acc_metric.reset_states()
            train_loss_metric.reset_states()
            test_loss_metric.reset_states()

            print(
                f"Train acc: {float(train_acc):.4f}, Train loss: {float(train_loss):.4f} --- Test acc: {float(test_acc):.4f}, Test loss: {float(test_loss):.4f}"
            )

    except ModuleNotFoundError:
        from logging import warning

        warning("test_loss_function_tensorflow requires tensorflow")
