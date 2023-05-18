import json
import unittest
from pathlib import Path

import numpy as np
import tensorflow as tf
import tensorflow.keras as keras

if __name__ == "__main__" and __package__ is None:
    from os.path import dirname as dir
    from sys import path

    path.append(dir(path[0]))

from vehicle_lang_tensorflow import generate_loss_function


class TestLossFunctionTranslation(unittest.TestCase):
    def load_json(self, file_name: str) -> str:
        path_to_json = Path(__file__).parent / "data" / file_name
        with open(path_to_json) as f:
            json_dict = json.load(f)
        return json_dict

    def vcl_file(self, file_name: str) -> str:
        return str((Path(__file__).parent / "data" / file_name).with_suffix(".vcl"))

    def _assert_tensor_equal(self, t1, t2):
        self.assertEqual(np.array_equal(np.array(t1), np.array(t2)), True)

    def test_constant(self):
        path_to_vcl = self.vcl_file("test_constant")
        functionName = "constant"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 5)

    def test_variable(self):
        path_to_vcl = self.vcl_file("test_variable")
        functionName = "variable"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(2), 2)

    def test_tensor(self):
        path_to_vcl = self.vcl_file("test_tensor")
        functionName = "tensor"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self._assert_tensor_equal(loss(), tf.constant([5, 2, 16, 7]))

    def test_negation(self):
        path_to_vcl = self.vcl_file("test_negation")
        functionName = "negation"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), -5)

    def test_minimum(self):
        path_to_vcl = self.vcl_file("test_minimum")
        functionName = "minimum"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 0)

    def test_maximum(self):
        path_to_vcl = self.vcl_file("test_maximum")
        functionName = "maximum"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 4)

    def test_addition(self):
        path_to_vcl = self.vcl_file("test_addition")
        functionName = "addition"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 8)

    def test_subtraction(self):
        path_to_vcl = self.vcl_file("test_subtraction")
        functionName = "subtraction"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 4)

    def test_multiplication(self):
        path_to_vcl = self.vcl_file("test_multiplication")
        functionName = "multiplication"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 12)

    # def test_division(self):
    #     path_to_vcl = self.vcl_file("test_division")
    #     functionName = "division"
    #     networks = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, networks)
    #     self.assertEqual(loss(), 3)

    # def test_power(self):
    #     path_to_vcl = self.vcl_file("test_power")
    #     functionName = "power"
    #     networks = {}
    #     loss = generate_loss_function(path_to_vcl, functionName, networks)
    #     self.assertEqual(loss(), 3)

    def test_indicator(self):
        path_to_vcl = self.vcl_file("test_indicator")
        functionName = "indicator"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 1)

    def test_at(self):
        path_to_vcl = self.vcl_file("test_at")
        functionName = "at"
        networks = {}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss([1, 4, 7]), 4)

    def test_network(self):
        path_to_vcl = self.vcl_file("test_network")
        functionName = "net_prop"
        model = keras.Sequential(
            [
                keras.layers.Input(shape=(1,)),
                keras.layers.Dense(units=1, kernel_initializer="ones"),
            ]
        )
        networks = {"net": model}
        loss = generate_loss_function(path_to_vcl, functionName, networks)
        self.assertEqual(loss(), 0)

    def test_forall_quantifier(self):
        path_to_vcl = self.vcl_file("test_quantifier_all")
        functionName = "quantifierForall"
        networks = {}

        quantifier_sampling = {"x": lambda: 2.1}
        loss = generate_loss_function(
            path_to_vcl, functionName, networks, quantifier_sampling=quantifier_sampling
        )
        self.assertEqual(loss(), 0)

        quantifier_sampling = {"x": lambda: -5.5}
        loss = generate_loss_function(
            path_to_vcl, functionName, networks, quantifier_sampling=quantifier_sampling
        )
        self.assertEqual(loss(), 5.5)

    def test_exists_quantifier(self):
        path_to_vcl = self.vcl_file("test_quantifier_any")
        functionName = "quantifierExists"
        networks = {}

        quantifier_sampling = {"x": lambda: 21.5}
        loss = generate_loss_function(
            path_to_vcl, functionName, networks, quantifier_sampling=quantifier_sampling
        )
        self.assertEqual(loss(), 0)

        quantifier_sampling = {"x": lambda: -2.8}
        loss = generate_loss_function(
            path_to_vcl, functionName, networks, quantifier_sampling=quantifier_sampling
        )
        self.assertEqual(loss(), 2.8)


if __name__ == "__main__":
    unittest.main()
