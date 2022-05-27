from typing import Dict
import tensorflow as tf
import numpy as np
import random
import json


def generate_loss_function(pathToSpec:str, functionName:str, resources:Dict[str,any]):
    '''
    pathToSpec: path to the vehicle spec .vcl file
    functionName: name of the function for which we want to create the loss function
    resources: dictionary mapping from the name of the resources declared in the spec to the python implementation
    '''

    jsonDICT = generateJson(pathToSpec, functionName)
    context = []
    loss = '_'
    loss = translateJSONtoLOSSfunction(resources, jsonDICT)

    return loss(context)
    

def generateJson(pathToSpec, functionName):
    #call vehicle executable on the provided spec file - for now just load a specific json file
    pathToJSON = '/Users/marcocasadio/Projects/vehicle/test/Test/Compile/Golden/bounded/bounded-output.json'
    f = open(pathToJSON)
    jsonDICT = json.load(f)
    f.close()
    return jsonDICT


def translateJSONtoLOSSfunction(resources, jsonDICT):
    if isinstance(jsonDICT, dict):
        tag = jsonDICT['tag']
        contents = jsonDICT['contents']

        if tag == 'Con':
            def result_func(context):
                return contents

            return result_func

        elif tag == 'Var':
            def result_func(context):
                return context[contents]

            return result_func

        elif tag == 'TensorLit':
            lossFuncList = []
            for c in contents:
                lossFuncList.append(translateJSONtoLOSSfunction(resources, c))

            def result_func(context):
                tensorOfValues = []
                for l in lossFuncList:
                    tensorOfValues.append(l(context))
                return tf.convert_to_tensor(tensorOfValues)

            return result_func

        elif tag == 'Neg':
            loss = translateJSONtoLOSSfunction(resources, contents)

            def result_func(context):
                return -loss(context)

            return result_func

        elif tag == 'Min':
            loss1 = translateJSONtoLOSSfunction(resources, contents[0])
            loss2 = translateJSONtoLOSSfunction(resources, contents[1])

            def result_func(context):
                return min(loss1(context), loss2(context))

            return result_func

        elif tag == 'Max':
            loss1 = translateJSONtoLOSSfunction(resources, contents[0])
            loss2 = translateJSONtoLOSSfunction(resources, contents[1])

            def result_func(context):
                return max(loss1(context), loss2(context))

            return result_func

        elif tag == 'Sub':
            loss1 = translateJSONtoLOSSfunction(resources, contents[0])
            loss2 = translateJSONtoLOSSfunction(resources, contents[1])

            def result_func(context):
                return loss1(context) - loss2(context)

            return result_func

        elif tag == 'Ind':
            loss1 = translateJSONtoLOSSfunction(resources, contents[0])
            loss2 = translateJSONtoLOSSfunction(resources, contents[1])

            def result_func(context):
                if loss1(context) == loss2(context):
                    return 1
                else:
                    return 0

            return result_func

        elif tag == 'At':
            lossList = contents[0]
            index = contents[1]
            lossIndex = translateJSONtoLOSSfunction(resources, index)
            lossTensor = translateJSONtoLOSSfunction(resources, lossList)

            def result_func(context):
                return lossTensor(context)[lossIndex(context)]

            return result_func
            
        elif tag == 'NetApp':      
            model = resources[contents[0]]
            lossInputsList = []
            for c in contents[1]:
                lossInputsList.append(translateJSONtoLOSSfunction(resources, c))

            def result_func(context):
                inputs = []
                for l in lossInputsList:
                    inputs.append(l(context))
                output = model(inputs, training=True)
                return output

            return result_func

        elif tag == 'Quant':
            body = contents[3]
            bodyloss = translateJSONtoLOSSfunction(resources, body)
            domainloss = translateJSONtoLOSSfunction(resources, contents[2])

            if contents[0] == 'All':
                def result_func(context):
                    domain = domainloss(context)
                    maxLoss = np.NINF
                    for _ in range(10):
                        context.insert(0, random.uniform(domain[0], domain[1]))
                        value = bodyloss(context)
                        if value > maxLoss:
                            maxLoss = value
                        context.pop(0)
                    return maxLoss

            elif contents[0] == 'Any':
                def result_func(context):
                    domain = domainloss(context)
                    minLoss = np.Inf
                    for _ in range(10):
                        context.insert(0, random.uniform(domain[0], domain[1]))
                        value = bodyloss(context)
                        if value < minLoss:
                            minLoss = value
                        context.pop(0)
                    return minLoss
                    
            return result_func

        elif tag == 'Lambda':
            body = contents[1]
            bodyloss = translateJSONtoLOSSfunction(resources, body)

            def result_func(context, v):
                context.insert(0, v)
                return bodyloss(context)

            return lambda context: lambda v: result_func(context, v)

        elif tag == 'Domain':
            loss1 = translateJSONtoLOSSfunction(resources, contents[0])
            loss2 = translateJSONtoLOSSfunction(resources, contents[1])

            def result_func(context):
                return loss1(context), loss2(context)

            return result_func
