from typing import Dict, Callable
from command_line import call_vehicle_to_generate_loss_json
import utils
import tensorflow as tf
import numpy as np
import random


def generate_loss_function(specification:str,
                            function_name:str,
                            resources:Dict[str,any],
                            quantifier_sampling:Dict[str,Callable]) -> Callable:
    '''
    pathToSpec: path to the vehicle spec .vcl file
    functionName: name of the function for which we want to create the loss function
    resources: dictionary mapping from the name of the resources declared in the spec to the python implementation
    '''
    json_dict = call_vehicle_to_generate_loss_json(specification, function_name)
    empty_context = []
    loss_metadata = LossMetadata(resources, quantifier_sampling)
    loss = LossFunctionTranslation().to_loss_function(loss_metadata, json_dict)

    #It seems that now loss(empty_context) gives a number instead of a function. Not really sure why.
    print(loss)
    return loss(empty_context)


class LossMetadata():
    def __init__(self, resources:Dict[str,any], quantifier_sampling:Dict[str,Callable]):
        self.resources = resources
        self.quantifier_sampling = quantifier_sampling


class LossFunctionTranslation:
    def to_loss_function(self, metadata:LossMetadata, json_dict:dict) -> Callable:
        declaration_context = {}
        #for _ in json_dict:
            #declaration_loss = self._translate_expression(resources, json_dict)
        declaration_loss = self._translate_expression(metadata, json_dict[1])
        return declaration_loss

    def _translate_expression(self, metadata:LossMetadata, json_dict:dict) -> Callable:
        tag = json_dict['tag']
        contents = json_dict['contents']

        # These strings are taken from the constructor names in src/hs/Vehicle/Backend/LossFunction/Compile.hs
        if tag == 'Constant': return self._translate_constant(contents)
        elif tag == 'Variable': return self._translate_variable(contents)
        elif tag == 'TensorLiteral': return self._translate_tensor(contents, metadata)
        elif tag == 'Negation': return self._translate_negation(contents, metadata)
        elif tag == 'Min': return self._translate_minimum(contents, metadata)
        elif tag == 'Max': return self._translate_maximum(contents, metadata)
        elif tag == 'Addition': return self._translate_addition(contents, metadata)
        elif tag == 'Subtraction': return self._translate_subtraction(contents, metadata)
        elif tag == 'Multiplication': return self._translate_multiplication(contents, metadata)
        elif tag == 'Division': return self._translate_division(contents, metadata)
        elif tag == 'IndicatorFunction': return self._translate_indicator(contents, metadata)
        elif tag == 'At': return self._translate_at(contents, metadata)
        elif tag == 'NetworkApplication': return self._translate_network(contents, metadata)
        elif tag == 'Quantifier': return self._translate_quantifier(contents, metadata)
        elif tag == 'Lambda': return self._translate_lambda(contents, metadata)
        elif tag == 'Domain': return self._translate_domain(contents, metadata)

    def _translate_constant(self, contents:dict) -> Callable:
        def result_func(context):
            return contents

        return result_func

    def _translate_variable(self, contents:dict) -> Callable:
        def result_func(context):
            return context[contents]

        return result_func
    
    def _translate_tensor(self, contents:dict, metadata:LossMetadata) -> Callable:
        func_losses = [self._translate_expression(metadata, c) for c in contents]

        def result_func(context):
            values_tensor = [l(context) for l in func_losses]
            return tf.convert_to_tensor(values_tensor)

        return result_func
    
    def _translate_negation(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss = self._translate_expression(metadata, contents)

        def result_func(context):
            return -loss(context)

        return result_func
    
    def _translate_minimum(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return min(loss_1(context), loss_2(context))

        return result_func

    def _translate_maximum(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return max(loss_1(context), loss_2(context))

        return result_func
    
    def _translate_addition(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return loss_1(context) + loss_2(context)

        return result_func

    def _translate_subtraction(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return loss_1(context) - loss_2(context)

        return result_func

    def _translate_multiplication(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return loss_1(context) * loss_2(context)

        return result_func

    def _translate_division(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return loss_1(context) / loss_2(context)

        return result_func

    def _translate_indicator(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return 1 if loss_1(context) == loss_2(context) else 0

        return result_func

    def _translate_at(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_list = contents[0]
        index = contents[1]
        loss_index = self._translate_expression(metadata, index)
        loss_tensor = self._translate_expression(metadata, loss_list)

        def result_func(context):
            return loss_tensor(context)[loss_index(context)]

        return result_func
    
    def _translate_network(self, contents:dict, metadata:LossMetadata) -> Callable:
        model = metadata.resources[contents[0]]
        input_losses = [self._translate_expression(metadata, c) for c in contents[1]]

        def result_func(context):
            inputs = [l(context) for l in input_losses]
            output = model(inputs, training=True)
            return output

        return result_func

    def _translate_quantifier(self, contents:dict, metadata:LossMetadata) -> Callable:
        quantifier = contents[0]
        variable_name = contents[1]
        domain = contents[2]
        body = contents[3]

        body_loss = self._translate_expression(metadata, body)

        if variable_name in metadata.quantifier_sampling:
            generate_sample = metadata.quantifier_sampling[variable_name]
        else:
            raise Exception('No sampling method provided for variable ' + variable_name + '.')

        def result_func(context):
            max_loss = np.NINF
            min_loss = np.Inf
            for _ in range(10):
                sample = generate_sample()
                context.insert(0, sample)
                if contents[0] == 'All':
                    max_loss = max(max_loss, body_loss(context))
                elif contents[0] == 'Any':
                    min_loss = min(min_loss, body_loss(context))
                context.pop(0)
            if contents[0] == 'All': return max_loss 
            elif contents[0] == 'Any': return min_loss
            else: utils.internal_error_msg('Found a quantifier in the generated json that is not All nor Any.')
                
        return result_func

    def _translate_lambda(self, contents:dict, metadata:LossMetadata) -> Callable:
        body = contents[1]
        body_loss = self._translate_expression(metadata, body)

        def result_func(context, v):
            context.insert(0, v)
            return body_loss(context)

        return lambda context: lambda v: result_func(context, v)

    def _translate_domain(self, contents:dict, metadata:LossMetadata) -> Callable:
        loss_1 = self._translate_expression(metadata, contents[0])
        loss_2 = self._translate_expression(metadata, contents[1])

        def result_func(context):
            return loss_1(context), loss_2(context)

        return result_func
