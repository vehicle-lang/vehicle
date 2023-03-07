import random

import numpy as np
import tensorflow as tf
import tensorflow.keras as keras
from keras.datasets import mnist

#model = keras.models.load_model('normal_training')

def get_constraint_accuracy(model, X_data, y_data, epsilon, delta):
    counter = 0
    X_preturbed = X_data + np.random.uniform(-epsilon, epsilon, X_data.shape)
    y_original = model.predict(X_data)
    y_preturbed = model.predict(X_preturbed)
    for i in range(y_original.shape[0]):
        if np.linalg.norm(y_original[i] - y_preturbed[i]) < delta:
            counter+=1
    return counter/X_data.shape[0]

(X_train, y_train), (X_test, y_test) = mnist.load_data()

# Scale images to the [0, 1] range
#X_train = X_train.astype("float32") / 255
#X_test = X_test.astype("float32") / 255

#y_train = keras.utils.to_categorical(y_train, 10)
#y_test = keras.utils.to_categorical(y_test, 10)

#train_dataset = tf.data.Dataset.from_tensor_slices((X_train, y_train))
#test_dataset = tf.data.Dataset.from_tensor_slices((X_test, y_test))

#indexes = np.random.randint(0, X_train.shape[0], size=2000)
#images = X_train[indexes]
#labels = y_train[indexes]
#model = keras.models.load_model('normal_training')

#we use saved model 
#result = get_constraint_accuracy(model, images, labels, 0.01, 0.02)
#print(result)
