from typing import List

from . import session, loss
from ._version import VERSION
from .compile import call_vehicle, compile_specification
from .error import VehicleError, VehicleInternalError
from .export import export_to_solver
from .list import list
from .session.error import VehicleSessionClosed, VehicleSessionUsed
from .typecheck import TypeSystem, typecheck
from .typing import (
    DeclarationName,
    DifferentiableLogic,
    ExportTarget,
    LossBackend,
    QuantifiedVariableName,
    QueryFormat,
    Verifier,
)
from .validate import validate
from .verify import verify

__all__: List[str] = [
    "VERSION",
    # Typecheck
    "TypeSystem",
    "typecheck",
    # Compile
    "compile_specification",
    # Loss helpers
    "loss",
    # Call Vehicle
    "call_vehicle",
    # Verify
    "verify",
    # Validate,
    "validate",
    # Export
    "export_to_solver",
    # List
    "list",
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
    "Verifier",
    "ExportTarget",
]
