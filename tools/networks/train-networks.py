import numpy as np
import tensorflow as tf
import tf2onnx
from keras.layers import Dense
from keras.models import Sequential
from numpy import asarray, float32


def identity_network():
    model = Sequential()
    kernel1 = [[1, 0], [0, 1]]
    initializer1 = tf.keras.initializers.Constant(kernel1)
    model.add(
        Dense(
            2,
            input_dim=2,
            activation=tf.identity,
            use_bias=False,
            kernel_initializer=initializer1,
        )
    )
    model.compile(loss="mse", optimizer="adam")
    return model


if __name__ == "__main__":
    model1 = identity_network()
    tf2onnx.convert.from_keras(model1, output_path="identity-2.onnx")
