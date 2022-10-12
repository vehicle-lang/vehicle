import unittest
import json
from vehicle import LossFunctionTranslation
from tensorflow import keras

class TestLossFunctionTranslation(unittest.TestCase):
    def load_json(self, file_name):
        path_to_json = f'./src/python/test_json/{file_name}.json'
        with open(path_to_json) as f:
            json_dict = json.load(f)
        return json_dict

    def test_constant(self):
        resources = {}
        json_dict = self.load_json('test_constant')
        context = []
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, 0)

    
    def test_variable(self):
        resources = {}
        json_dict = self.load_json('test_variable')
        context = [5, 2]
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, 2)
    

    def test_tensor(self):
        resources = {}
        json_dict = self.load_json('test_tensor')
        context = [5, 2]
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss.shape, (1,))
        self.assertEqual(loss, 5)


    def test_negation(self):
        resources = {}
        json_dict = self.load_json('test_negation')
        context = []
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, -1)


    def test_minimum(self):
        resources = {}
        json_dict = self.load_json('test_minimum')
        context = []
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, 7)


    def test_maximum(self):
        resources = {}
        json_dict = self.load_json('test_maximum')
        context = []
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, 12)


    # def test_addition(self):
    #     resources = {}
    #     json_dict = self.load_json('test_addition')
    #     context = []
    #     loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
    #     self.assertEqual(loss, 5)


    def test_subtraction(self):
        resources = {}
        json_dict = self.load_json('test_subtraction')
        context = []
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, 5)
    

    # def test_multiplication(self):
    #     resources = {}
    #     json_dict = self.load_json('test_multiplication')
    #     context = []
    #     loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
    #     self.assertEqual(loss, 5)
    

    # def test_division(self):
    #     resources = {}
    #     json_dict = self.load_json('test_division')
    #     context = []
    #     loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
    #     self.assertEqual(loss, 5)


    def test_indicator(self):
        resources = {}
        json_dict = self.load_json('test_indicator')
        context = []
        loss_not_equal = LossFunctionTranslation().to_loss_function(resources, json_dict[0])(context)
        loss_equal = LossFunctionTranslation().to_loss_function(resources, json_dict[1])(context)
        self.assertEqual(loss_not_equal, 0)
        self.assertEqual(loss_equal, 1)


    def test_at(self):
        resources = {}
        json_dict = self.load_json('test_at')
        context = [[4, 13, 22, 1]]
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, 22)


    def test_network(self):
        model = keras.Sequential([
            keras.layers.Input(shape=(1,)),
            keras.layers.Dense(units=1, kernel_initializer='ones'),
        ])
        resources = {'f': model}
        json_dict = self.load_json('test_network')
        context = [2, 3.5, 14]
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, 3.5)


    def test_quantifier(self):
        resources = {}
        json_dict = self.load_json('test_quantifier')
        context = []
        loss_all = LossFunctionTranslation().to_loss_function(resources, json_dict[0])(context)
        loss_any = LossFunctionTranslation().to_loss_function(resources, json_dict[1])(context)
        self.assertEqual(loss_all, 50)
        self.assertEqual(loss_any, 12)

    
    def test_lambda(self):
        resources = {}
        json_dict = self.load_json('test_lambda')
        context = []
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)(3)
        self.assertEqual(loss, 3)
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)(8)
        self.assertEqual(loss, 8)
    

    def test_domain(self):
        resources = {}
        json_dict = self.load_json('test_domain')
        context = []
        loss = LossFunctionTranslation().to_loss_function(resources, json_dict)(context)
        self.assertEqual(loss, (4, 7))


if __name__ == '__main__':
    unittest.main()