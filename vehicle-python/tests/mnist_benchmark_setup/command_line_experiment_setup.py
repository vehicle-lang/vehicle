import json
import shutil
import subprocess
from tempfile import TemporaryDirectory
from typing import Any, Dict, List

#for experiments
import os

# Function that calls vehicle - it takes the options
# TODO: check if it's None or else
def call_vehicle(args: List[str]) -> None:
    vehicle = shutil.which("vehicle")
    if vehicle is None:
        raise Exception(f"Could not find vehicle on PATH; is vehicle installed?")
    command = " ".join([vehicle] + args) #passed as string due to shell=True
    result = subprocess.run(command, capture_output=True, shell=True) #shell = True is needed for Windows
    if result.returncode != 0:
        errorMessage = f"Problem during compilation: {result.stderr.decode('UTF-8')}"
        commandMessage = f"Command was: {command}"
        raise Exception(errorMessage + commandMessage)


def load_json(path_to_json: str) -> Dict[Any, Any]:
    with open(path_to_json) as f:
        json_dict = json.load(f)
    return json_dict


def call_vehicle_to_generate_loss_json(
    specification: str,
    networks: Dict[str, Any],
    datasets: Dict[str, Any],
    parameters: Dict[str, Any],
    function_name: str,
) -> Dict[Any, Any]:
    with TemporaryDirectory() as path_to_json_directory:
        path_to_json = path_to_json_directory + "loss_function.json"
        args = [
            "compile",
            "--target",
            # use the string below to specify which translation is used. the options are:
            #LossFunction (uses DL2 by default), LossFunction-DL2, LossFunction-Godel, LossFunction-Lukasiewicz,
            #  LossFunction-Product, LossFunction-STL (in progress, not currently working)
            #"LossFunction",
             # for experiments,
            os.getenv('DLV'),
            "--specification",
            specification,
            "--outputFile",
            path_to_json,
        ]
        args += make_resource_arguments({k: "N/A" for k in networks}, "network")
        args += make_resource_arguments(datasets, "dataset")
        args += make_resource_arguments(parameters, "parameter")
        args += ["--declaration", function_name]
        call_vehicle(args)
        loss_function_json = load_json(path_to_json)

    return loss_function_json


def make_resource_arguments(resources: Dict[str, str], arg_name: str) -> List[str]:
    resource_list = []
    for name, resource in resources.items():
        resource_list.append("--" + arg_name)
        resource_list.append(name + ":" + str(resource))
    return resource_list


def call_vehicle_to_verify_specification(
    specification: str,
    verifier: str,
    networks: Dict[str, str],
    datasets: Dict[str, str],
    parameters: Dict[str, Any],
) -> None:
    network_list = make_resource_arguments({k: "N/A" for k in networks}, "network")
    dataset_list = make_resource_arguments(datasets, "dataset")
    parameter_list = make_resource_arguments(parameters, "parameter")
    args = ["verify", "--specification", specification, "--verifier", verifier]
    args = args + network_list + dataset_list + parameter_list
    call_vehicle(args)
