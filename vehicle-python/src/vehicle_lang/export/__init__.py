from pathlib import Path
from typing import Optional, Union

from .. import session
from ..error import VehicleError as VehicleError
from ..typing import ExportTarget


def export_to_solver(
    target: ExportTarget,
    output_file: Optional[Union[str, Path]] = None,
    module_name: Optional[str] = None,
    cache: Optional[Union[str, Path]] = None,
) -> str:
    """
    Export a.vcl specification file to an interactive theorem prover.

    :param target: The target format to export to (only Agda supported currently).
    :param output_file: Output location for the compiled file(s). Defaults to stdout if not provided.
    :param module_name: Override the name of the exported module (for ITP targets).
    :param cache: The location of the verification cache for ITP compilation.
    """

    args = ["export", "--target", str(target._vehicle_option_name)]

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
    exc, out, err, log = session.check_output(args)

    # Check for errors or return
    if exc != 0:
        raise VehicleError(err or out or log or "unknown error")
    elif not out:
        raise VehicleError("no output")
    else:
        return out
