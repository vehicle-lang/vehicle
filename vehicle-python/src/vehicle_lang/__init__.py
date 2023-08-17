from typing import List

from ._version import VERSION as VERSION
from .compile.error import VehicleBuiltinUnsupported as VehicleBuiltinUnsupported
from .compile.error import VehiclePropertyNotFound as VehiclePropertyNotFound
from .compile.python import load_loss_function as load_loss_function
from .error import VehicleError as VehicleError
from .error import VehicleInternalError as VehicleInternalError
from .session.error import VehicleSessionClosed as VehicleSessionClosed
from .session.error import VehicleSessionUsed as VehicleSessionUsed
from .typing import AnyOptimiser as AnyOptimiser
from .typing import AnyOptimisers as AnyOptimisers
from .typing import DeclarationName as DeclarationName
from .typing import DifferentiableLogic as DifferentiableLogic
from .typing import Optimiser as Optimiser
from .typing import QuantifiedVariableName as QuantifiedVariableName
from .typing import Verifier as Verifier
from .verify import verify as verify

__all__: List[str] = [
    "VERSION",
    # Compile
    "load_loss_function",
    # Verify
    "verify",
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
    "Verifier",
]
