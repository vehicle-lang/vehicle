from vehicle import generate_loss_function
from tensorflow import keras
import tensorflow as tf
import numpy as np
import random


def train(model, train_dataset, test_dataset, epochs, alfa, beta, path_to_spec, functionName, resources, quantifier_sampling):
    optimizer = keras.optimizers.Adam()
    ce_batch_loss = keras.losses.BinaryCrossentropy()
    vehicle_batch_loss = generate_loss_function(path_to_spec, functionName, resources, quantifier_sampling)
    #Yep, I receive a number instead of a function.
    #If I pass loss instead of loss(empty_context) it works if I do
    #vehicle_loss = vehicle_batch_loss([]) instead of vehicle_loss = vehicle_batch_loss(0.5)
    #Is this because we don't have the Lambda at the beginning of the json?
    #Or is it because we don't pass the network yet, so it is a just a number not related to the batch?
    print(vehicle_batch_loss)

    train_acc_metric = keras.metrics.BinaryCrossentropy()
    test_acc_metric = keras.metrics.BinaryCrossentropy()
    train_loss_metric = keras.metrics.BinaryCrossentropy()
    test_loss_metric = keras.metrics.BinaryCrossentropy()

    for epoch in range(epochs):
        print(f"\nEpoch {epoch + 1}")

        # Iterate over the batches of the dataset.
        for x_batch_train, y_batch_train in train_dataset:
            # Open a GradientTape to record the operations run during the forward pass, which enables auto-differentiation.
            with tf.GradientTape() as tape:
                outputs = model(x_batch_train, training=True)  # Outputs for this minibatch
                ce_loss_value = ce_batch_loss(y_batch_train, outputs)
                vehicle_loss = vehicle_batch_loss(0.5)
                total_loss = ce_loss_value * alfa + vehicle_loss * beta
            # Use the gradient tape to automatically retrieve the gradients of the trainable variables with respect to the loss.
            grads = tape.gradient(total_loss, model.trainable_weights)
            # Run one step of gradient descent by updating the value of the variables to minimize the loss.
            optimizer.apply_gradients(zip(grads, model.trainable_weights))
        
        # Run a training loop at the end of each epoch.
        for x_batch_train, y_batch_train in train_dataset:
            train_outputs = model(x_batch_train, training=False)
            train_acc_metric.update_state(y_batch_train, train_outputs)
            train_loss_metric.update_state(y_batch_train, train_outputs)

        # Run a testing loop at the end of each epoch.
        for x_batch_test, y_batch_test in test_dataset:
            test_outputs = model(x_batch_test, training=False)
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

        print(f"Train acc: {float(train_acc):.4f}, Train loss: {float(train_loss):.4f} --- Test acc: {float(test_acc):.4f}, Test loss: {float(test_loss):.4f}")

    return model


if __name__ == '__main__':
    path_to_spec = '/Users/marcocasadio/Projects/vehicle/test/specs/bounded.vcl'
    function_name = 'bounded'
    model = keras.Sequential([
        keras.layers.Input(shape=(1,)),
        keras.layers.Dense(units=1),
    ])
    resources = {'f': model}

    quantifier_sampling = {'x': lambda: random.uniform(0, 1)}

    batch_size = 1
    epochs = 4
    alfa = 0
    beta = 1

    X_train = np.array([[0.0], [0.2], [0.4], [0.6], [0.8]])
    X_test = np.array([[0.1], [0.3], [0.5], [0.7], [0.9]])
    y_train = np.array([0, 0, 0, 1, 1])
    y_test = np.array([0, 0, 1, 1, 1])

    train_dataset = tf.data.Dataset.from_tensor_slices((X_train, y_train))
    test_dataset = tf.data.Dataset.from_tensor_slices((X_test, y_test))

    train_dataset = train_dataset.shuffle(buffer_size=1024).batch(batch_size)
    test_dataset = test_dataset.batch(batch_size)
    
    model = train(model, train_dataset, test_dataset, epochs, alfa, beta, path_to_spec, function_name, resources, quantifier_sampling)
