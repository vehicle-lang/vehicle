from typing import Dict, Callable
import utils
import tensorflow as tf
import numpy as np
import random
import json


def generate_loss_function(path_to_spec:str, function_name:str, resources:Dict[str,any]) -> Callable:
    '''
    pathToSpec: path to the vehicle spec .vcl file
    functionName: name of the function for which we want to create the loss function
    resources: dictionary mapping from the name of the resources declared in the spec to the python implementation
    '''
    # Call command_line.call_vehicle_to_generate_loss_json()
    #json_dict = load_json(path_to_spec)
    empty_context = []
    loss = LossFunctionTranslation().to_loss_function(resources, json_dict)

    return loss(empty_context)


class LossFunctionTranslation:
    def to_loss_function(self, resources:Dict[str,any], json_dict:dict) -> Callable:
        tag = json_dict['tag']
        contents = json_dict['contents']

        if tag == 'Con': return self._translate_constant(contents)
        elif tag == 'Var': return self._translate_variable(contents)
        elif tag == 'TensorLit': return self._translate_tensor(contents, resources)
        elif tag == 'Neg': return self._translate_negation(contents, resources)
        elif tag == 'Min': return self._translate_minimum(contents, resources)
        elif tag == 'Max': return self._translate_maximum(contents, resources)
        elif tag == 'Sub': return self._translate_subtraction(contents, resources)
        elif tag == 'Ind': return self._translate_indicator(contents, resources)
        elif tag == 'At': return self._translate_at(contents, resources)
        elif tag == 'NetApp': return self._translate_network(contents, resources)
        elif tag == 'Quant': return self._translate_quantifier(contents, resources)
        elif tag == 'Lambda': return self._translate_lambda(contents, resources)
        elif tag == 'Domain': return self._translate_domain(contents, resources)

    def _translate_constant(self, contents:dict) -> Callable:
        def result_func(context):
            return contents

        return result_func

    def _translate_variable(self, contents:dict) -> Callable:
        def result_func(context):
            return context[contents]

        return result_func
    
    def _translate_tensor(self, contents:dict, resources:Dict[str,any]) -> Callable:
        func_losses = [self.to_loss_function(resources, c) for c in contents]

        def result_func(context):
            values_tensor = [l(context) for l in func_losses]
            return tf.convert_to_tensor(values_tensor)

        return result_func
    
    def _translate_negation(self, contents:dict, resources:Dict[str,any]) -> Callable:
        loss = self.to_loss_function(resources, contents)

        def result_func(context):
            return -loss(context)

        return result_func
    
    def _translate_minimum(self, contents:dict, resources:Dict[str,any]) -> Callable:
        loss_1 = self.to_loss_function(resources, contents[0])
        loss_2 = self.to_loss_function(resources, contents[1])

        def result_func(context):
            return min(loss_1(context), loss_2(context))

        return result_func

    def _translate_maximum(self, contents:dict, resources:Dict[str,any]) -> Callable:
        loss_1 = self.to_loss_function(resources, contents[0])
        loss_2 = self.to_loss_function(resources, contents[1])

        def result_func(context):
            return max(loss_1(context), loss_2(context))

        return result_func
    
    def _translate_subtraction(self, contents:dict, resources:Dict[str,any]) -> Callable:
        loss_1 = self.to_loss_function(resources, contents[0])
        loss_2 = self.to_loss_function(resources, contents[1])

        def result_func(context):
            return loss_1(context) - loss_2(context)

        return result_func

    def _translate_indicator(self, contents:dict, resources:Dict[str,any]) -> Callable:
        loss_1 = self.to_loss_function(resources, contents[0])
        loss_2 = self.to_loss_function(resources, contents[1])

        def result_func(context):
            return 1 if loss_1(context) == loss_2(context) else 0

        return result_func

    def _translate_at(self, contents:dict, resources:Dict[str,any]) -> Callable:
        loss_list = contents[0]
        index = contents[1]
        loss_index = self.to_loss_function(resources, index)
        loss_tensor = self.to_loss_function(resources, loss_list)

        def result_func(context):
            return loss_tensor(context)[loss_index(context)]

        return result_func
    
    def _translate_network(self, contents:dict, resources:Dict[str,any]) -> Callable:
        model = resources[contents[0]]
        input_losses = [self.to_loss_function(resources, c) for c in contents[1]]

        def result_func(context):
            inputs = [l(context) for l in input_losses]
            output = model(inputs, training=True)
            return output

        return result_func

    def _translate_quantifier(self, contents:dict, resources:Dict[str,any]) -> Callable:
        body = contents[3]
        body_loss = self.to_loss_function(resources, body)
        domain_loss = self.to_loss_function(resources, contents[2])

        def result_func(context):
            domain = domain_loss(context)
            max_loss = np.NINF
            min_loss = np.Inf
            for _ in range(10):
                context.insert(0, random.uniform(domain[0], domain[1]))
                if contents[0] == 'All':
                    max_loss = max(max_loss, body_loss(context))
                elif contents[0] == 'Any':
                    min_loss = min(min_loss, body_loss(context))
                context.pop(0)
            if contents[0] == 'All': return max_loss 
            elif contents[0] == 'Any': return min_loss
            else: utils.internal_error_msg('Found a quantifier in the generated json that is not All nor Any.')
                
        return result_func

    def _translate_lambda(self, contents:dict, resources:Dict[str,any]) -> Callable:
        body = contents[1]
        body_loss = self.to_loss_function(resources, body)

        def result_func(context, v):
            context.insert(0, v)
            return body_loss(context)

        return lambda context: lambda v: result_func(context, v)

    def _translate_domain(self, contents:dict, resources:Dict[str,any]) -> Callable:
        loss_1 = self.to_loss_function(resources, contents[0])
        loss_2 = self.to_loss_function(resources, contents[1])

        def result_func(context):
            return loss_1(context), loss_2(context)

        return result_func
