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
class NetworkSummary:
    provenance: Provenance
    name: str
    typeText: str


@dataclass(frozen=True)
class DatasetSummary:
    provenance: Provenance
    name: str
    typeText: str


@dataclass(frozen=True)
class ParameterSummary:
    provenance: Provenance
    name: str
    typeText: str
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
    provenance: Provenance
    name: str
    typeText: str
    quantifier: Quantifier


@dataclass(frozen=True)
class PropertySummary:
    provenance: Provenance
    name: str
    typeText: str
    quantifiedVariables: Optional[MultiPropertyTree[list[QuantifiedVariableSummary]]]


@dataclass(frozen=True)
class SpecificationSummary:
    networks: List[NetworkSummary]
    datasets: List[DatasetSummary]
    parameters: List[ParameterSummary]
    properties: List[PropertySummary]


def list_entities(specification: str | Path) -> SpecificationSummary:
    """
    List all networks, datasets, parameters, and properties in the specification.

    :param specification: The path to the Vehicle specification file to list entities for.
    :return: A summary of all entities in the specification.
    """
    args = ["--json", "list", "entities", "--specification", str(specification)]

    # Call Vehicle
    out = session.execute_command(args)
    if out is None:
        raise VehicleInternalError("Vehicle did not return any output")

    try:
        return decode(SpecificationSummary, json.loads(out))
    except Exception as exc:
        raise VehicleInternalError(str(exc)) from exc


__all__ = [
    "SpecificationSummary",
    "NetworkSummary",
    "DatasetSummary",
    "ParameterSummary",
    "PropertySummary",
    "SingleProperty",
    "MultiProperty",
    "MultiPropertyTree",
    "QuantifiedVariableSummary",
    "list_entities",
]
