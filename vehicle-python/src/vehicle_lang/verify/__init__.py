from pathlib import Path
from typing import Any, Dict, Iterable, Optional, Union

from .. import session
from ..error import VehicleError
from ..typing import DeclarationName, Verifier


def verify(
    specification: Union[str, Path],
    properties: Optional[Iterable[DeclarationName]] = None,
    networks: Dict[DeclarationName, Union[str, Path]] = {},
    datasets: Dict[DeclarationName, Union[str, Path]] = {},
    parameters: Dict[DeclarationName, Any] = {},
    verifier: Verifier = Verifier.Marabou,
    verifier_location: Optional[Union[str, Path]] = None,
    cache: Optional[Union[str, Path]] = None,
) -> None:
    """
    Check whether properties in a Vehicle specification hold.

    :param specification: The path to the Vehicle specification file to verify.
    :param properties: The names of the properties in the specification to verify, defaults to all declarations.
    :param networks: A map from the network names in the specification to files containing the networks.
    :param datasets: A map from the dataset names in the specification to files containing the datasets.
    :param parameters: A map from the parameter names in the specification to the values to be used in verification.
    :param verifier: The verifier to be used, defaults to Marabou.
    :param verifier_location: The path to the verifier executable, defaults to searching the system path.
    :param cache: The path to the proof cache used by Vehicle, defaults to not writing a proof cache.
    """
    args = ["verify", "--specification", str(specification)]

    if properties is not None:
        for property_name in set(properties):
            args.extend(["--property", property_name])

    for network_name, network_path in networks.items():
        args.extend(["--network", f"{network_name}:{network_path}"])

    for dataset_name, dataset_path in datasets.items():
        args.extend(["--dataset", f"{dataset_name}:{dataset_path}"])

    for parameter_name, parameter_value in parameters.items():
        args.extend(["--parameter", f"{parameter_name}:{parameter_value}"])

    args.extend(["--verifier", verifier._vehicle_option_name])

    if verifier_location is not None:
        args.extend(["--verifier-location", str(verifier_location)])

    if cache is not None:
        args.extend(["--cache", str(cache)])

    # Call Vehicle
    exc, out, err, log = session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(err or out or log or "unknown error")
    if out is None:
        raise VehicleError("no output")
    return None
