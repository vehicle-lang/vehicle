import json
import subprocess
from tempfile import TemporaryDirectory
from typing import Any, List


# Function that calls vehicle - it takes the options
# TODO: check if it's None or else
def call_vehicle(args: List[str]) -> None:
    command = ["vehicle"] + args
    # print(' '.join(command))
    result = subprocess.run(command, capture_output=True)
    if result.returncode != 0:
        raise Exception(
            "Error during specification compilation: " + result.stderr.decode("UTF-8")
        )


def load_json(path_to_json: str) -> dict:
    with open(path_to_json) as f:
        json_dict = json.load(f)
    return json_dict


def call_vehicle_to_generate_loss_json(specification: str, function_name: str) -> dict:
    with TemporaryDirectory() as path_to_json_directory:
        path_to_json = path_to_json_directory + "loss_function.json"
        args = [
            "compile",
            "--target",
            "LossFunction",
            "--specification",
            specification,
            "--outputFile",
            path_to_json,
        ]
        #'--property', function_name]
        call_vehicle(args)
        loss_function_json = load_json(path_to_json)
    return loss_function_json


def make_resource_arguments(resources: dict[str, str], arg_name: str) -> List[str]:
    resource_list = []
    for name, resource in resources.items():
        resource_list.append("--" + arg_name)
        resource_list.append(name + ":" + str(resource))
    return resource_list


def call_vehicle_to_verify_specification(
    specification: str,
    verifier: str,
    networks: dict[str, str],
    datasets: dict[str, str],
    parameters: dict[str, Any],
) -> None:
    network_list = make_resource_arguments(networks, "network")
    dataset_list = make_resource_arguments(datasets, "dataset")
    parameter_list = make_resource_arguments(parameters, "parameter")
    args = ["verify", "--specification", specification, "--verifier", verifier]
    args = args + network_list + dataset_list + parameter_list
    call_vehicle(args)
    return
