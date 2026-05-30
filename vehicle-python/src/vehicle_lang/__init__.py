from typing import List

from . import loss, session
from ._version import VERSION
from .compile import call_vehicle, compile_specification
from .error import VehicleError, VehicleInternalError, VehicleUserError
from .export import export_to_solver
from .list import list_entities
from .session.error import VehicleSessionClosed, VehicleSessionUsed
from .typecheck import SecondaryTypeSystem, typecheck, typecheck_with_typesystem
from .typing import (
    DeclarationName,
    DifferentiableLogic,
    ExportTarget,
    LossBackend,
    QuantifiedVariableName,
    QueryFormat,
)
from .validate import validate
from .verify import (
    MultiPropertyFinish,
    MultiPropertyStart,
    ProgressEvent,
    PropertyAddress,
    PropertyFinish,
    PropertyStart,
    QueryAddress,
    QueryError,
    QueryFinish,
    QueryStart,
    VerificationFinish,
    VerificationStart,
    decode_progress_event,
    verify,
)

__all__: List[str] = [
    "VERSION",
    # Typecheck
    "SecondaryTypeSystem",
    "typecheck",
    "typecheck_with_typesystem",
    # Compile
    "compile_specification",
    # Loss helpers
    "loss",
    # Call Vehicle
    "call_vehicle",
    # Verify
    "verify",
    "decode_progress_event",
    "PropertyAddress",
    "QueryAddress",
    "ProgressEvent",
    "VerificationStart",
    "VerificationFinish",
    "MultiPropertyStart",
    "MultiPropertyFinish",
    "PropertyStart",
    "PropertyFinish",
    "QueryStart",
    "QueryFinish",
    "QueryError",
    # Validate,
    "validate",
    # Export
    "export_to_solver",
    # List
    "list_entities",
    # Session
    "session",
    # Error types
    "VehicleError",
    "VehicleSessionClosed",
    "VehicleSessionUsed",
    "VehicleInternalError",
    "VehicleUserError",
    # Argument types
    "DeclarationName",
    "QuantifiedVariableName",
    "DifferentiableLogic",
    "LossBackend",
    "QueryFormat",
    "ExportTarget",
]
