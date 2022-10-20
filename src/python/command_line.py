import subprocess
from tempfile import TemporaryDirectory
from typing import List
import json


# Function that calls vehicle - it takes the options
# TODO: check if it's None or else
def call_vehicle(args:List[str]) -> None:
    command = ['vehicle'] + args
    # print(' '.join(command))
    result = subprocess.run(command, capture_output=True)
    if result.returncode != 0:
        raise Exception('Error during specification compilation: ' + result.stderr.decode('UTF-8'))


def load_json(path_to_json:str) -> dict:
    with open(path_to_json) as f:
        json_dict = json.load(f)
    return json_dict


def call_vehicle_to_generate_loss_json(specification:str, function_name:str) -> dict:
    with TemporaryDirectory() as path_to_json_directory:
        path_to_json = path_to_json_directory + 'loss_function.json'
        args =  ['compile',
                '--target', 'LossFunction', 
                '--specification', specification, 
                '--outputFile', path_to_json,
                '--property', function_name]
        call_vehicle(args)
        loss_function_json = load_json(path_to_json)
    return loss_function_json


def make_network_argument(networks:dict[str, str]) -> List[str]:
    network_list = []
    for name, location in networks.items():
        network_list.append('--network')
        network_list.append(name + ':' + location)
    return network_list


def make_dataset_argument(datasets:dict[str, str]) -> List[str]:
    dataset_list = []
    for name, location in datasets.items():
        dataset_list.append('--network')
        dataset_list.append(name + ':' + location)
    return dataset_list


def make_parameter_argument(parameters:dict[str, any]) -> List[str]:
    parameter_list = []
    for name, value in parameters.items():
        parameter_list.append('--network')
        parameter_list.append(name + ':' + str(value))
    return parameter_list


def call_vehicle_to_verify_specification(specification:str, verifier:str, networks:dict[str, str], datasets:dict[str, str], parameters:dict[str, any]) -> None:
    network_list = make_network_argument(networks)
    dataset_list = make_dataset_argument(datasets)
    parameter_list = make_parameter_argument(parameters)
    args =  ['verify', 
            '--specification', specification, 
            '--verifier', verifier]
    args = args + network_list + dataset_list + parameter_list
    call_vehicle(args)
    return
