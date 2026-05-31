import json
from abc import ABCMeta
from dataclasses import dataclass
from pathlib import Path
from typing import Generic, List, Optional

from typing_extensions import TypeAlias
from typing_extensions import TypeVar as TypingTypeVar

from .. import session
from .._ast._decode import JsonValue, decode
from .._ast._nodes import Provenance
from ..error import VehicleInternalError

Quantifier: TypeAlias = str


@dataclass(frozen=True)
class SharedData:
    provenance: Provenance
    name: str
    typeText: str


@dataclass(frozen=True)
class NetworkSummary:
    sharedData: SharedData


@dataclass(frozen=True)
class DatasetSummary:
    sharedData: SharedData


@dataclass(frozen=True)
class ParameterSummary:
    sharedData: SharedData
    inferable: bool


_T = TypingTypeVar("_T")


@dataclass(frozen=True, init=False)
class MultiPropertyTree(Generic[_T], metaclass=ABCMeta):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class MultiPropertyTree")


@dataclass(frozen=True)
class SingleProperty(MultiPropertyTree[_T]):
    value: _T


@dataclass(frozen=True)
class MultiProperty(MultiPropertyTree[_T]):
    values: list[MultiPropertyTree[_T]]


@dataclass(frozen=True)
class QuantifiedVariableSummary:
    sharedData: SharedData
    quantifier: Quantifier


@dataclass(frozen=True)
class PropertySummary:
    sharedData: SharedData
    subcomponents: Optional[MultiPropertyTree[list[QuantifiedVariableSummary]]]


@dataclass(frozen=True, init=False)
class ListableEntity(metaclass=ABCMeta):
    def __init__(self) -> None:
        raise TypeError("Cannot instantiate abstract class ListableEntity")


@dataclass(frozen=True)
class Network(ListableEntity):
    summary: NetworkSummary


@dataclass(frozen=True)
class Dataset(ListableEntity):
    summary: DatasetSummary


@dataclass(frozen=True)
class Parameter(ListableEntity):
    summary: ParameterSummary


@dataclass(frozen=True)
class Property(ListableEntity):
    summary: PropertySummary


def _decode_listable_entities(value: JsonValue) -> List[ListableEntity]:
    return decode(List[ListableEntity], value)


def list_entities(specification: str | Path) -> List[ListableEntity]:
    """
    List all networks, datasets, parameters, and properties in the specification.

    :param specification: The path to the Vehicle specification file to list entities for.
    :return: list of structured listable entities.
    """
    args = ["list", "--specification", str(specification), "--json"]

    # Call Vehicle
    out = session.execute_command(args)
    if not out:
        return []

    try:
        return _decode_listable_entities(json.loads(out))
    except Exception as exc:
        raise VehicleInternalError(str(exc)) from exc


__all__ = [
    "Dataset",
    "DatasetSummary",
    "ListableEntity",
    "MultiProperty",
    "MultiPropertyTree",
    "Network",
    "NetworkSummary",
    "Parameter",
    "ParameterSummary",
    "Property",
    "PropertySummary",
    "Provenance",
    "QuantifiedVariableSummary",
    "Quantifier",
    "SharedData",
    "SingleProperty",
    "list_entities",
]
