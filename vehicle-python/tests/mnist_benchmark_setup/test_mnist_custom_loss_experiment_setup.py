import random

import numpy as np
import tensorflow as tf
import tensorflow.keras as keras
from keras.datasets import mnist
from pathlib import Path

from vehicle import generate_loss_function
from mnist_constraint_accuracy import get_constraint_accuracy


def train(
    model,
    train_dataset,
    test_dataset,
    epochs,
    alfa,
    beta,
    path_to_spec,
    function_name,
    networks,
    datasets,
    parameters,
    quantifier_sampling,
):
    optimizer = keras.optimizers.Adam()
    ce_batch_loss = keras.losses.CategoricalCrossentropy()
    vehicle_batch_loss = generate_loss_function(
        specification=path_to_spec,
        function_name=function_name,
        networks=networks,
        datasets=datasets,
        parameters=parameters,
        quantifier_sampling=quantifier_sampling,
    )

    train_acc_metric = keras.metrics.CategoricalAccuracy()
    test_acc_metric = keras.metrics.CategoricalAccuracy()
    train_loss_metric = keras.metrics.CategoricalCrossentropy()
    test_loss_metric = keras.metrics.CategoricalCrossentropy()

    for epoch in range(epochs):
        print(f"\nEpoch {epoch + 1}")
        start_time = time.time()
        # Iterate over the batches of the dataset.
        for x_batch_train, y_batch_train in train_dataset:
            
            # Open a GradientTape to record the operations run during the forward pass, which enables auto-differentiation.
            with tf.GradientTape() as tape:
                # Outputs for this minibatch
                outputs = model(x_batch_train, training=True)
                ce_loss_value = ce_batch_loss(y_batch_train, outputs)
                vehicle_loss = vehicle_batch_loss()
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

        print(
            f"Train acc: {float(train_acc):.4f}, Train loss: {float(train_loss):.4f} --- Test acc: {float(test_acc):.4f}, Test loss: {float(test_loss):.4f}"
        )
        dataFile = open("classification-data.csv", "a")
        dataFile.write(str(epoch + 1) + "," + os.getenv('DLV') + "," + os.getenv('RATIOV') + "," + str(float(test_acc)) + "\n")
        dataFile.close()

    return model


if __name__ == "__main__":
    print("Starting")
    path_to_spec = Path(__file__).parent / "mnist.vcl" 
    path_to_spec = path_to_spec.__str__()
    #path_to_spec = "vehicle-python/tests/mnist.vcl"
    function_name = "robust1"
    model = keras.Sequential(
        [
            keras.Input(shape=(28, 28)),
            keras.layers.Flatten(),
            keras.layers.Dense(30, activation="relu"),
            keras.layers.Dense(10, activation="softmax"),
        ]
    )

    networks = {"mnist": model}

    batch_size = 64
    # epochs = 10
    # alfa = 1
    # beta = 1
    epochs = int(os.getenv('EPOCHV'))
    alfa = 100 - int(os.getenv('RATIOV'))
    beta = int(os.getenv('RATIOV'))

    (X_train, y_train), (X_test, y_test) = mnist.load_data()

    # Scale images to the [0, 1] range
    X_train = X_train.astype("float32") / 255
    X_test = X_test.astype("float32") / 255
    # Make sure images have shape (28, 28, 1)
    # X_train = np.expand_dims(X_train, -1)
    # X_test = np.expand_dims(X_test, -1)

    y_train = keras.utils.to_categorical(y_train, 10)
    y_test = keras.utils.to_categorical(y_test, 10)

    train_dataset = tf.data.Dataset.from_tensor_slices((X_train, y_train))
    test_dataset = tf.data.Dataset.from_tensor_slices((X_test, y_test))

    train_dataset = train_dataset.shuffle(buffer_size=1024).batch(batch_size)
    test_dataset = test_dataset.batch(batch_size)
    
    quantifier_sampling = {
        "x": lambda: random.choice(X_train),
        "j": lambda: random.randint(0, 27),
        "i": lambda: random.randint(0, 27),
        "k": lambda: random.randint(0, 9),
        "pertubation": lambda: np.random.uniform(low=-0.1, high=0.01, size=(28, 28)),
    }


    model = train(
        model,
        train_dataset,
        test_dataset,
        epochs,
        alfa,
        beta,
        path_to_spec,
        function_name,
        networks,
        {},
        {},
        quantifier_sampling,
    )

    #---- uncomment if you'd like to save the trained model
    #model.save('dl2_training')
    

    #---- uncomment to get constraint accuracy for the model 

    indexes = np.random.randint(0, X_train.shape[0], size=1000)
    images = X_train[indexes]
    labels = y_train[indexes]

    #indexes = np.random.randint(0, X_train.shape[0], size=1000)
    #images = X_train[indexes]
    #labels = y_train[indexes]

    #epsilon = 0.01
    #delta = 0.02
    epsilon = 0.01
    # delta = 0.02

    #constraint_acc = get_constraint_accuracy(model, images, labels, epsilon, delta)
    #print(constraint_acc)
    dataFile = open("constraint-data.csv", "a")
    deltas = [0.015,0.016,0.017,0.018,0.019,0.02,0.021,0.022,0.023,0.024,0.025]
    for delta in deltas:
        constraint_acc = get_constraint_accuracy(model, images, labels, epsilon, delta)
        print("delta=", delta, ", constraint satisfaction=", constraint_acc)
        dataFile.write(str(int(os.getenv('EPOCHV'))) + "," + os.getenv('DLV') + "," + os.getenv('RATIOV') + "," + str(delta) + "," + str(float(constraint_acc)) + "\n")

    dataFile.close()

