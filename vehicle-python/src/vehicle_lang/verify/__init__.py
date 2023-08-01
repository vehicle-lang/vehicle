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
) -> str:
    """
    Check whether a Vehicle specification is true.

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
        for property_name in set(properties):
            args.extend(["--property", property_name])

    for network_name, network_path in networks.items():
        args.extend(["--network", f"{network_name}:{network_path}"])

    for dataset_name, dataset_path in datasets.items():
        args.extend(["--dataset", f"{dataset_name}:{dataset_path}"])

    for parameter_name, parameter_value in parameters.items():
        args.extend(["--parameter", f"{parameter_name}:{parameter_value}"])

    args.extend(["--verifier", verifier.vehicle_option_name])

    if verifier_location is not None:
        args.extend(["--verifierLocation", str(verifier_location)])

    if cache is not None:
        args.extend(["--cache", str(cache)])

    # Call Vehicle
    exc, out, err, log = session.check_output(args)

    # Check for errors
    if exc != 0:
        raise VehicleError(err or out or log or "unknown error")
    if out is None:
        raise VehicleError("no output")
    return out
