import numpy as np
import matplotlib.pyplot as plt
from keras.layers import Dense, Flatten
from keras.models import Sequential
from keras.utils import to_categorical
from keras.datasets import mnist
import tensorflow as tf
import tensorflow.keras as keras
import time

(X_train, y_train), (X_test, y_test) = mnist.load_data()

# Scale images to the [0, 1] range
X_train = X_train.astype("float32") / 255
X_test = X_test.astype("float32") / 255
# Make sure images have shape (28, 28, 1)
# X_train = np.expand_dims(X_train, -1)
# X_test = np.expand_dims(X_test, -1)

y_train = keras.utils.to_categorical(y_train, 10)
y_test = keras.utils.to_categorical(y_test, 10)

# Create simple Neural Network model
model = keras.Sequential(
    [
        keras.Input(shape=(28, 28)),
        keras.layers.Reshape((28, 28, 1), input_shape=(28, 28)),
        keras.layers.Conv2D(32, kernel_size=(3, 3), activation="relu"),
        keras.layers.MaxPooling2D(pool_size=(2, 2)),
        keras.layers.Conv2D(64, kernel_size=(3, 3), activation="relu"),
        keras.layers.MaxPooling2D(pool_size=(2, 2)),
        keras.layers.Flatten(),
        keras.layers.Dropout(0.5),
        keras.layers.Dense(10, activation="softmax"),
    ]
)

model.summary()

model.compile(loss='categorical_crossentropy', 
	      optimizer='adam',
	      metrics=['acc'])

start_time = time.time()
# Train the Neural Network model
model.fit(X_train, y_train, epochs=5, validation_data=(X_test,y_test))
print("Time: %s seconds ---" % (time.time() - start_time))
# Making predictions using our trained model
#predictions = model.predict(X_test)
#predictions = np.argmax(predictions, axis=1)
