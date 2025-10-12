from typing import List

from ._version import VERSION as VERSION
from .check import check as check
from .compile import compile_to_queries as compile_to_queries
from .compile.error import VehicleBuiltinUnsupported as VehicleBuiltinUnsupported
from .compile.error import VehiclePropertyNotFound as VehiclePropertyNotFound

# from .compile.python import load_loss_function as load_loss_function
from .error import VehicleError as VehicleError
from .error import VehicleInternalError as VehicleInternalError
from .export import export_to_solver as export_to_solver
from .list import list as list
from .session.error import VehicleSessionClosed as VehicleSessionClosed
from .session.error import VehicleSessionUsed as VehicleSessionUsed
from .typing import AnyOptimiser as AnyOptimiser
from .typing import AnyOptimisers as AnyOptimisers
from .typing import DeclarationName as DeclarationName
from .typing import DifferentiableLogic as DifferentiableLogic
from .typing import ExportTarget as ExportTarget
from .typing import Optimiser as Optimiser
from .typing import QuantifiedVariableName as QuantifiedVariableName
from .typing import QueryFormat as QueryFormat
from .typing import TypeSystem as TypeSystem
from .typing import Verifier as Verifier
from .validate import validate as validate
from .verify import verify as verify

__all__: List[str] = [
    "VERSION",
    # Check
    "check",
    # Compile
    "load_loss_function",
    "compile_to_queries",
    # Verify
    "verify",
    # Validate,
    "validate",
    # Export
    "export_to_solver",
    # List
    "list",
    # Error types
    "VehicleError",
    "VehicleSessionClosed",
    "VehicleSessionUsed",
    "VehicleBuiltinUnsupported",
    "VehicleInternalError",
    "VehiclePropertyNotFound",
    # Argument types
    "DeclarationName",
    "QuantifiedVariableName",
    "Optimiser",
    "AnyOptimiser",
    "AnyOptimisers",
    "DifferentiableLogic",
    "QueryFormat",
    "Verifier",
    "TypeSystem",
    "ExportTarget",
]
