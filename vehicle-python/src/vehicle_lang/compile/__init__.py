from pathlib import Path
from typing import Any, Dict, Iterable, Optional, Union

from .. import session
from ..error import VehicleError as VehicleError
from ..typing import DeclarationName, DifferentiableLogic, QueryFormat


def compile_to_query(
    path: Union[str, Path],
    target: Union[DifferentiableLogic, QueryFormat],
    declarations: Optional[Iterable[DeclarationName]] = None,
    networks: Dict[DeclarationName, Union[str, Path]] = {},
    datasets: Dict[DeclarationName, Union[str, Path]] = {},
    parameters: Dict[DeclarationName, Any] = {},
    output_file: Optional[Union[str, Path]] = None,
    module_name: Optional[str] = None,
    cache: Optional[Union[str, Path]] = None,
) -> str:
    """
    Compile a Vehicle specification to a query language (e.g., Marabou, VNNLIB etc.)

    :param specification: The path to the Vehicle specification file to compile.
    :param target: The target language to compile to (e.g., QueryFormat.Marabou).
    :param declarations: The names of the declarations to compile, defaults to all declarations.
    :param networks: A map from the network names in the specification to files containing the networks.
    :param datasets: A map from the dataset names in the specification to files containing the datasets.
    :param parameters: A map from the parameter names in the specification to the values to be used in compilation.
    :param output_file: Output location for the compiled file(s). Defaults to stdout if not provided.
    :param module_name: Override the name of the exported module (for ITP targets).
    :param cache: The location of the verification cache for ITP compilation.
    """

    args = [
        "compile",
        "--specification",
        str(path),
        "--target",
        target._vehicle_option_name,
    ]

    # Add declarations if specified
    if declarations is not None:
        for declaration_name in set(declarations):
            args.extend(["--declaration", declaration_name])

    # Add networks, datasets, and parameters
    for network_name, network_path in networks.items():
        args.extend(["--network", f"{network_name}:{network_path}"])

    for dataset_name, dataset_path in datasets.items():
        args.extend(["--dataset", f"{dataset_name}:{dataset_path}"])

    for parameter_name, parameter_value in parameters.items():
        args.extend(["--parameter", f"{parameter_name}:{parameter_value}"])

    # Add output file if specified
    if output_file is not None:
        args.extend(["--output", str(output_file)])

    # Add module name if specified
    if module_name is not None:
        args.extend(["--module-name", module_name])

    # Add cache if specified
    if cache is not None:
        args.extend(["--cache", str(cache)])

    # Call Vehicle
    exec, out, err, _ = session.check_output(args)

    if exec != 0:
        raise VehicleError(f"{err}")
    elif not out:
        raise VehicleError(f"Vehicle produced no output")

    return out
