import random

import numpy as np
import tensorflow as tf
import tensorflow.keras as keras
from keras.datasets import mnist

# model = keras.models.load_model('normal_training')

loss_object = tf.keras.losses.CategoricalCrossentropy()


def create_adversarial_pattern(pretrained_model, input_image, input_label):
    one_image = np.expand_dims(input_image, axis=0)
    one_image = tf.convert_to_tensor(one_image)
    with tf.GradientTape() as tape:
        tape.watch(one_image)
        prediction = pretrained_model(one_image)
        loss = loss_object(tf.reshape(input_label, (1, 10)), prediction)

    # Get the gradients of the loss w.r.t to the input image.
    gradient = tape.gradient(loss, one_image)

    # Get the sign of the gradients to create the perturbation
    signed_grad = tf.sign(gradient)
    return signed_grad


def get_constraint_accuracy_fgsm(model, images, labels, num_labels, epsilon, delta):
    counter = 0
    # fgsm_epsilon=0.1, weight=90 --> 0.003 constraint satisfaction
    # fgsm_epsilon = 0.01, weight=90 --> 0.476 constraint satisfaction
    # fgsm_epsilon = 0.005, weight=95, epochs=5 --> 0.541 constraint satisfaction
    # fgsm_epsilon = 0.005, weight=95, epochs=10 --> 0.56 constraint satisfaction
    # fgsm_epsilon = 0.002, weight=95, epochs=5 --> 0.712 constraint satisfaction
    fgsm_epsilon = 0.002

    i = 0
    for image in images:
        print("generating perturbed image " + str(i) + " of " + str(num_labels))
        perturbations = create_adversarial_pattern(model, image, labels[i])
        adversarial_image = image + fgsm_epsilon * perturbations
        label_original_image = model.predict(np.expand_dims(image, axis=0))
        label_perturbed_image = model.predict(adversarial_image)
        if np.linalg.norm(label_original_image - label_perturbed_image) < delta:
            counter += 1
        i = i + 1
    return counter / num_labels


def get_constraint_accuracy(model, X_data, y_data, epsilon, delta):
    counter = 0
    X_preturbed = X_data + np.random.uniform(-epsilon, epsilon, X_data.shape)
    y_original = model.predict(X_data)
    y_preturbed = model.predict(X_preturbed)
    for i in range(y_original.shape[0]):
        if np.linalg.norm(y_original[i] - y_preturbed[i]) < delta:
            counter += 1
    return counter / X_data.shape[0]


# (X_train, y_train), (X_test, y_test) = mnist.load_data()

# Scale images to the [0, 1] range
# X_train = X_train.astype("float32") / 255
# X_test = X_test.astype("float32") / 255

# y_train = keras.utils.to_categorical(y_train, 10)
# y_test = keras.utils.to_categorical(y_test, 10)

# train_dataset = tf.data.Dataset.from_tensor_slices((X_train, y_train))
# test_dataset = tf.data.Dataset.from_tensor_slices((X_test, y_test))

# indexes = np.random.randint(0, X_train.shape[0], size=2000)
# images = X_train[indexes]
# labels = y_train[indexes]
# model = keras.models.load_model('normal_training')

# we use saved model
# result = get_constraint_accuracy(model, images, labels, 0.01, 0.02)
# print(result)
