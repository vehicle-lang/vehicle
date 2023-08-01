import ast as py
from dataclasses import asdict, dataclass, field
from enum import Enum
from functools import reduce
from pathlib import Path
from typing import Any, Callable, Dict, Optional, Set, Union

from typing_extensions import TypeAlias, final, overload, override
from vehicle_lang._error import VehicleError
from vehicle_lang.ast import Program
from vehicle_lang.compile.abc import AnyBuiltins, Sampler
from vehicle_lang.compile.python import PythonBuiltins, PythonTranslation

from . import session as global_session

DeclarationName: TypeAlias = str
"""
A name of a top-level declaration in a Vehicle specification (.vcl) file.
"""

QuantifiedVariableName: TypeAlias = str
"""
A name of a quantified variable in a Vehicle specification (.vcl) file.
"""


class Verifier(Enum):
    """
    An enumeration of the neural network verifiers supported by Vehicle.
    """

    Marabou = 1
    """
    The Marabou verifier (https://github.com/NeuralNetworkVerification/Marabou) as
    described in:

    Katz, Guy, et al. "The marabou framework for verification and analysis of deep neural networks."
    Computer Aided Verification: 31st International Conference, CAV 2019, New York City, NY, USA,
    July 15-18, 2019, Proceedings, Part I 31. Springer International Publishing, 2019.
    """

    @property
    def vehicle_command_line_interface_name(self) -> str:
        return {
            Verifier.Marabou: "Marabou",
        }[self]


def verify(
    specification: Union[str, Path],
    properties: Optional[Set[DeclarationName]] = None,
    networks: Optional[Dict[DeclarationName, Union[str, Path]]] = None,
    datasets: Optional[Dict[DeclarationName, Union[str, Path]]] = None,
    parameters: Optional[Dict[DeclarationName, Any]] = None,
    verifier: Verifier = Verifier.Marabou,
    verifier_location: Optional[Union[str, Path]] = None,
    proof_cache: Optional[Union[str, Path]] = None,
) -> str:
    """
    Checks whether a Vehicle specification is true.

    Parameters
    ----------
    specification -- the path to the Vehicle specification (.vcl) file to verify.

    properties -- a set of names of `@property` declarations in the specification
    to verify. If not provided, then all `@property` declarations are verified.

    networks -- a map from the names of the `@network` declarations in the specification
    file to the file paths of the `.onnx` files containing the actual networks.

    datasets -- a map from the names of the `@dataset` declarations in the specification
    file to the file paths of the `.idx` files containing the actual datasets with which
    to perform the verification.

    parameters -- a map from the names of the `@parameter` declarations in the specification
    file to the values that should be used during the verification.

    verifier -- the neural network verifier to use to perform the verification. This should
    either be available on the system `PATH` variable or the installation location of the
    verifier executable needs to be passed explicitly via the `verifier_location` argument.

    verifier_location -- the installation location of the verifier executable. Not needed
    if the verifier is available via the system `PATH` variable.

    proof_cache -- the location to output the proof cache for the verification result.
    Useful when exporting a proof to an interactive theorem prover. If not provided then
    no proof cache is written.

    Returns
    -------
    At the moment, this function does not return anything particularly useful, but merely
    outputs the string output by the Vehicle compiler. This will change hopefully
    in the near future.
    """
    args = ["verify", "--specification", str(specification)]

    if properties is not None:
        for property_name in properties:
            args.extend(["--property", str(property_name)])

    if networks is not None:
        for network_name, network_path in networks.items():
            args.extend(["--network", f"{network_name}:{network_path}"])

    if datasets is not None:
        for dataset_name, dataset_path in datasets.items():
            args.extend(["--dataset", f"{dataset_name}:{dataset_path}"])

    if parameters is not None:
        for parameter_name, parameter_value in parameters.items():
            args.extend(["--parameter", f"{parameter_name}:{parameter_value}"])

    args.extend(["--verifier", verifier.vehicle_command_line_interface_name])

    if verifier_location is not None:
        args.extend(["--verifierLocation", str(verifier_location)])

    if proof_cache is not None:
        args.extend(["--proofCache", str(proof_cache)])

    # Call Vehicle
    exc, out, err, log = global_session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(err or out or log or "unknown error")
    if out is None:
        raise VehicleError("no output")
    return out


class DifferentiableLogic(Enum):
    """
    An enumeration of the differentiable logics supported by Vehicle.
    """

    VEHICLE = 1
    DL2 = 2
    GODEL = 3
    LUKASIEWICZ = 4
    PRODUCT = 5
    YAGER = 6

    @property
    def vehicle_command_line_interface_name(self) -> str:
        return {
            DifferentiableLogic.VEHICLE: "VehicleLoss",
            DifferentiableLogic.DL2: "DL2Loss",
            DifferentiableLogic.GODEL: "GodelLoss",
            DifferentiableLogic.LUKASIEWICZ: "LukasiewiczLoss",
            DifferentiableLogic.PRODUCT: "ProductLoss",
            DifferentiableLogic.YAGER: "YagerLoss",
        }[self]


def generate_loss_function(
    specification: Union[str, Path],
    differentiable_logic: DifferentiableLogic = DifferentiableLogic.VEHICLE,
    samplers: Optional[Dict[QuantifiedVariableName, Sampler[Any]]] = None,
) -> Dict[str, Any]:
    """
    Generate a loss function from a property in a Vehicle specification.

    Parameters
    ----------
    specification -- the file path to the Vehicle specification file (.vcl file).

    differentiable_logic -- the differentiable logic that Vehicle should use for
    the translation to a loss function.

    samplers -- a dictionary mapping names of quantified variables in the specification
    to methods for sampling for their value. See the `Sampler` documentation for more
    details.

    Returns
    -------

    A mapping from declaration names to python functions that accept all the dependent
    external resources in the specification as keyword arguments and return a value
    representing the `loss`, i.e. how false the property is.
    """

    # Call Vehicle compile
    output = compile(
        specification=specification,
        target=differentiable_logic.vehicle_command_line_interface_name,
        as_json=True,
    )

    # Load the AST from the generated JSON
    program = Program.from_json(output)

    # Instantiate the translation
    if samplers is None:
        samplers = {}
    translation = PythonTranslation(builtins=PythonBuiltins(samplers=samplers))

    # Translate the specification AST to a Python module:
    return translation.compile(
        program=program, filename=str(specification), declaration_context={}
    )


def generate_python_function(specification: Union[str, Path]) -> Dict[str, Any]:
    """
    Generates an executable Python version of a Vehicle specification.

    Parameters
    ----------
    specification -- the file path to the Vehicle specification file (.vcl file).

    differentiable_logic -- the differentiable logic that Vehicle should use for
    the translation to a loss function.

    Returns
    -------

    A python module representation of the specification, where individual functions
    can be accessed using dictionary lookup.
    """

    # Call Vehicle compile
    output = compile(specification=specification, target="Explicit", as_json=True)

    # Translate the program from an AST.
    program = Program.from_json(output)

    # Translate the specification to a Python module:
    translation = PythonTranslation(builtins=PythonBuiltins())
    return translation.compile(
        program=program, filename=str(specification), declaration_context={}
    )


def compile(
    target: str,
    specification: Union[str, Path],
    output_file: Optional[Union[str, Path]] = None,
    declarations: Optional[Set[DeclarationName]] = None,
    networks: Optional[Dict[DeclarationName, Path]] = None,
    datasets: Optional[Dict[DeclarationName, Path]] = None,
    parameters: Optional[Dict[DeclarationName, Any]] = None,
    module_name: Optional[str] = None,
    cache: Optional[str] = None,
    as_json: bool = False,
) -> str:
    # Ensure that specification_path is a Path
    if isinstance(specification, str):
        specification = Path(specification)

    # Ensure that output file is a Path
    if isinstance(output_file, str):
        output_file = Path(output_file)

    args = ["compile"]
    args.extend(["--target", target])
    args.extend(["--specification", str(specification)])

    if output_file is not None:
        args.extend(["--outputFile", str(output_file)])

    if declarations is not None:
        for declaration_name in declarations:
            args.extend(["--declaration", declaration_name])

    if networks is not None:
        for network_name, network_path in networks.items():
            args.extend(["--network", f"{network_name}:{network_path}"])

    if datasets is not None:
        for dataset_name, dataset_path in datasets.items():
            args.extend(["--dataset", f"{dataset_name}:{dataset_path}"])

    if parameters is not None:
        for parameter_name, parameter_value in parameters.items():
            args.extend(["--parameter", f"{parameter_name}:{parameter_value}"])

    if module_name is not None:
        args.extend(["--moduleName", module_name])

    if cache is not None:
        args.extend(["--cache", cache])

    if as_json:
        args.append("--json")

    exc, out, err, log = global_session.check_output(args)
    if exc != 0:
        raise VehicleError(err or out or log or "unknown error")
    if out is None:
        raise VehicleError("no output")
    return out
