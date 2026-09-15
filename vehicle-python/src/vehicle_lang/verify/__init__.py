import json
from abc import ABCMeta
from ast import Name
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, List, Optional

from typing_extensions import TypeAlias

from .. import session
from .._ast._decode import JsonValue, decode
from ..error import VehicleInternalError
from ..typing import DeclarationName

###############
## Addresses ##
###############


@dataclass
class PropertyAddress:
    propertyName: str
    propertyIndices: List[int]


@dataclass
class QueryAddress:
    queryID: int
    property: PropertyAddress


#####################
## Progress events ##
#####################


@dataclass(init=False)
class ProgressEvent(metaclass=ABCMeta):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class ProgressEvent")


@dataclass
class VerificationStart(ProgressEvent):
    pass


@dataclass
class VerificationFinish(ProgressEvent):
    pass


@dataclass
class MultiPropertyStart(ProgressEvent):
    propertyName: str


@dataclass
class MultiPropertyFinish(ProgressEvent):
    propertyName: str


@dataclass
class PropertyStart(ProgressEvent):
    propertyAddress: PropertyAddress


@dataclass
class PropertyFinish(ProgressEvent):
    propertyAddress: PropertyAddress
    verified: bool


@dataclass
class QueryStart(ProgressEvent):
    queryAddress: QueryAddress


@dataclass
class QueryFinish(ProgressEvent):
    queryAddress: QueryAddress
    satisfied: bool


@dataclass
class QueryError(ProgressEvent):
    queryAddress: QueryAddress
    errorMessage: str


def decode_progress_event(json: JsonValue) -> ProgressEvent:
    return decode(ProgressEvent, json)


def parse_progress_events(output: str) -> List[ProgressEvent]:
    """
    Parse a string of concatenated JSON objects into a list of ProgressEvent.

    :param output: A string containing one or more JSON objects (not a JSON array).
    :returns: A list of decoded ProgressEvent objects.
    """
    decoder = json.JSONDecoder()
    events: List[ProgressEvent] = []
    pos = 0
    while pos < len(output):
        # Skip whitespace between JSON objects
        while pos < len(output) and output[pos].isspace():
            pos += 1
        if pos >= len(output):
            break
        obj, end = decoder.raw_decode(output, pos)
        events.append(decode_progress_event(obj))
        pos = end
    return events


###################
## Verify method ##
###################


def verify(
    specification: str | Path,
    solver: str | Path,
    solver_args: Optional[List[str]] = None,
    properties: Optional[Iterable[DeclarationName]] = None,
    networks: dict[DeclarationName, str | Path] = {},
    datasets: dict[DeclarationName, str | Path] = {},
    parameters: dict[DeclarationName, Any] = {},
    cache: Optional[str | Path] = None,
) -> List[ProgressEvent]:
    """
    Check whether properties in a Vehicle specification hold and returns a list of progress events.

    It is guaranteed that:
        - The first event is a VerificationStart event.
        - The last event is a VerificationFinish event.
        - For every MultiPropertyStart event, there is a corresponding MultiPropertyFinish event with the same propertyName.
        - For every PropertyStart event, there is a corresponding PropertyFinish or QueryError event with the same property.
        - For every QueryStart event, there is a corresponding QueryFinish or QueryError event with the same queryAddress.
        - Higher-level property events (VerificationStart, MultiPropertyStart, PropertyStart) always start before lower-level property events, and finish after lower-level property events. For example, if a PropertyStart event for property P occurs between a MultiPropertyStart event for property P and its corresponding MultiPropertyFinish event, then the corresponding PropertyFinish event for property P also occurs between the same MultiPropertyStart and MultiPropertyFinish events.
    Other than that, the order is not guaranteed, and in particular, events from different properties may be interleaved in any order.

    NOTE: this should really return a live stream of progress events, but at
    the moment only returns when the verification finishes.

    :param specification: The path to the Vehicle specification file or Vehicle to verify.
    :param solver: The path to the solver executable, or the name of the executable on the system path.
    :param solver_args: A list of extra arguments to pass to the solver, defaults to no extra arguments.
    :param properties: The names of the properties in the specification to verify, defaults to all declarations.
    :param networks: A map from the network names in the specification to files containing the networks.
    :param datasets: A map from the dataset names in the specification to files containing the datasets.
    :param parameters: A map from the parameter names in the specification to the values to be used in verification.
    :param cache: The path to the proof cache used by Vehicle, defaults to not writing a proof cache.

    :returns: A list of progress events from Vehicle.

    """
    args = [
        "--json",
        "verify",
        "--specification",
        str(specification),
        "--solver",
        str(solver),
    ]

    if solver_args is not None:
        args.extend(["--solver-args", " ".join(solver_args)])

    if properties is not None:
        for property_name in set(properties):
            args.extend(["--property", property_name])

    for network_name, network_path in networks.items():
        args.extend(["--network", f"{network_name}:{network_path}"])

    for dataset_name, dataset_path in datasets.items():
        args.extend(["--dataset", f"{dataset_name}:{dataset_path}"])

    for parameter_name, parameter_value in parameters.items():
        args.extend(["--parameter", f"{parameter_name}:{parameter_value}"])

    if cache is not None:
        args.extend(["--cache", str(cache)])

    # Call Vehicle
    out = session.execute_command(args)
    if not out:
        raise VehicleInternalError("Vehicle produced no output")

    return parse_progress_events(out)
