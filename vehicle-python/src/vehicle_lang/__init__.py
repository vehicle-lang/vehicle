from typing import List

from ._version import VERSION as VERSION
from .compile.error import VehicleBuiltinUnsupported as VehicleBuiltinUnsupported
from .compile.error import VehiclePropertyNotFound as VehiclePropertyNotFound
from .compile.python import load_loss_function as load_loss_function
from .compile.variabledomain import BoundedVariableDomain as BoundedVariableDomain
from .error import VehicleError as VehicleError
from .error import VehicleInternalError as VehicleInternalError
from .session.error import VehicleSessionClosed as VehicleSessionClosed
from .session.error import VehicleSessionUsed as VehicleSessionUsed
from .typing import DeclarationName as DeclarationName
from .typing import DifferentiableLogic as DifferentiableLogic
from .typing import Domain as Domain
from .typing import Domains as Domains
from .typing import Optimiser as Optimiser
from .typing import Optimisers as Optimisers
from .typing import QuantifiedVariableName as QuantifiedVariableName
from .typing import VariableDomain as VariableDomain
from .typing import Verifier as Verifier
from .verify import verify as verify

__all__: List[str] = [
    "VERSION",
    # Compile
    "load_loss_function",
    "VariableDomain",
    "BoundedVariableDomain",
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
