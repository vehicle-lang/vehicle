from pathlib import Path
from typing import Any, Dict, Iterable, Optional, Union

from .. import session
from ..error import VehicleError as VehicleError
from ..typing import ITP, DeclarationName, DifferentiableLogic, QueryFormat


def compile(
    path: Union[str, Path],
    target: Union[DifferentiableLogic, QueryFormat, ITP],
    output_file: Union[str, Path],
    declarations: Optional[Iterable[DeclarationName]] = None,
    networks: Dict[DeclarationName, Union[str, Path]] = {},
    datasets: Dict[DeclarationName, Union[str, Path]] = {},
    parameters: Dict[DeclarationName, Any] = {},
    module_name: Optional[str] = None,
    cache: Optional[Union[str, Path]] = None,
) -> str:
    """
    Compile a Vehicle specification to a target language

    :param specification: The path to the Vehicle specification file to compile.
    :param target: The target language to compile to (e.g. QueryFormat.Marabou).
    :param output_file: Output location for the compiled file(s).
    :param declarations: The names of the declarations to compile, defaults to all declarations.
    :param networks: A map from the network names in the specification to files containing the networks.
    :param datasets: A map from the dataset names in the specification to files containing the datasets.
    :param parameters: A map from the parameter names in the specification to the values to be used in compilation.
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

    # Add output file
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


def compile_to_queries(
    path: Union[str, Path],
    target: QueryFormat,
    output_folder: Union[str, Path],
    declarations: Optional[Iterable[DeclarationName]] = None,
    networks: Dict[DeclarationName, Union[str, Path]] = {},
    datasets: Dict[DeclarationName, Union[str, Path]] = {},
    parameters: Dict[DeclarationName, Any] = {},
) -> str:
    """
    Compile a Vehicle specification to queries for a verifier. This is useful if you want to generate
    the queries but not run the verifier immediately.

    :param specification: The path to the Vehicle specification file to compile.
    :param target: The target language to compile to (e.g. QueryFormat.Marabou).
    :param output_folder: Output folder for the compiled file(s).
    :param declarations: The names of the declarations to compile, defaults to all declarations.
    :param networks: A map from the network names in the specification to files containing the networks.
    :param datasets: A map from the dataset names in the specification to files containing the datasets.
    :param parameters: A map from the parameter names in the specification to the values to be used in compilation.
    """
    return compile(
        path=path,
        target=target,
        declarations=declarations,
        networks=networks,
        datasets=datasets,
        parameters=parameters,
        output_file=output_folder,
    )
