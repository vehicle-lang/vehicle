import unittest
import json
from vehicle import LossFunctionTranslation
from vehicle import generate_loss_function
from tensorflow import keras
import tensorflow as tf
import numpy as np


def assert_tensor_equal(t1, t2):
    return (np.array(t1) & np.array(t2)).all()


class TestLossFunctionTranslation(unittest.TestCase):
    def load_json(self, file_name):
        path_to_json = f'./vehicle-python/src/vehicle/test_json/{file_name}.json'
        with open(path_to_json) as f:
            json_dict = json.load(f)
        return json_dict

    def vcl_file(self, file_name):
        path_to_vcl = f'./vehicle-python/src/vehicle/test_json/{file_name}.vcl'
        return path_to_vcl

    def test_constant(self):
        path_to_vcl = self.vcl_file('test_constant')
        functionName = 'constant'
        resources = {}
        loss = generate_loss_function(path_to_vcl, functionName, resources)
        self.assertEqual(loss(), 5)
    
    
    # def test_variable(self):
    #     path_to_vcl = self.vcl_file('test_variable')
    #     functionName = 'variable'
    #     resources = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, resources)
    #     self.assertEqual(loss(2), 2)
    

    def test_tensor(self):
        path_to_vcl = self.vcl_file('test_tensor')
        functionName = 'tensor'
        resources = {}
        loss = generate_loss_function(path_to_vcl, functionName, resources)
        self.assertEqual(assert_tensor_equal(loss().shape, tf.constant([2, 4, 1, 0]).shape), True)
        self.assertEqual(assert_tensor_equal(loss(), [5, 2, 16, 7]), True)
    

    # def test_negation(self):
    #     path_to_vcl = self.vcl_file('test_negation')
    #     functionName = 'negation'
    #     resources = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, resources)
    #     self.assertEqual(loss(), 0)


    # def test_minimum(self):
    #     path_to_vcl = self.vcl_file('test_minimum')
    #     functionName = 'minimum'
    #     resources = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, resources)
    #     self.assertEqual(loss(), 0)


    # def test_maximum(self):
    #     path_to_vcl = self.vcl_file('test_maximum')
    #     functionName = 'maximum'
    #     resources = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, resources)
    #     self.assertEqual(loss(), 1)


    def test_addition(self):
        path_to_vcl = self.vcl_file('test_addition')
        functionName = 'addition'
        resources = {}
        loss = generate_loss_function(path_to_vcl, functionName, resources)
        self.assertEqual(loss(), 8)


    def test_subtraction(self):
        path_to_vcl = self.vcl_file('test_subtraction')
        functionName = 'subtraction'
        resources = {}
        loss = generate_loss_function(path_to_vcl, functionName, resources)
        self.assertEqual(loss(), 4)
    

    def test_multiplication(self):
        path_to_vcl = self.vcl_file('test_multiplication')
        functionName = 'multiplication'
        resources = {}
        loss = generate_loss_function(path_to_vcl, functionName, resources)
        self.assertEqual(loss(), 12)
    

    def test_division(self):
        path_to_vcl = self.vcl_file('test_division')
        functionName = 'division'
        resources = {}
        loss = generate_loss_function(path_to_vcl, functionName, resources)
        self.assertEqual(loss(), 3)
 

    # def test_indicator(self):
    #     path_to_vcl = self.vcl_file('test_indicator')
    #     functionName = 'indicator'
    #     resources = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, resources)
    #     # loss(5)(7)
    #     self.assertEqual(loss(5, 7), 0)
    #     self.assertEqual(loss(5, 5), 1)


    # def test_at(self):
    #     path_to_vcl = self.vcl_file('test_at')
    #     functionName = 'at'
    #     resources = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, resources)
    #     self.assertEqual(loss([1, 4, 7]), 4)


    def test_network(self):
        path_to_vcl = self.vcl_file('test_network')
        functionName = 'network'
        model = keras.Sequential([
            keras.layers.Input(shape=(1,)),
            keras.layers.Dense(units=1, kernel_initializer='ones'),
        ])
        resources = {'net': model}
        loss = generate_loss_function(path_to_vcl, functionName, resources)
        self.assertEqual(loss(), 1)


    def test_quantifier(self):
        path_to_vcl = self.vcl_file('test_quantifier_all')
        functionName = 'quantifier'
        resources = {}

        quantifier_sampling = {'x': lambda: 2.1}
        loss = generate_loss_function(path_to_vcl, functionName, resources, quantifier_sampling)
        print(loss())
        self.assertEqual(loss(), 2.1)

        quantifier_sampling = {'x': lambda: -5.5}
        loss = generate_loss_function(path_to_vcl, functionName, resources, quantifier_sampling)
        self.assertEqual(loss(), -5.5)

        path_to_vcl = self.vcl_file('test_quantifier_any')

        quantifier_sampling = {'x': lambda: 21.5}
        loss = generate_loss_function(path_to_vcl, functionName, resources, quantifier_sampling)
        self.assertEqual(loss(), 21.5)

        quantifier_sampling = {'x': lambda: -2.8}
        loss = generate_loss_function(path_to_vcl, functionName, resources, quantifier_sampling)
        self.assertEqual(loss(), -2.8)


if __name__ == '__main__':
    unittest.main()