from typing import List

from .compile.error import VehicleBuiltinUnsupported as VehicleBuiltinUnsupported
from .compile.error import VehiclePropertyNotFound as VehiclePropertyNotFound
from .compile.tensorflow import TensorflowVariableDomain as TensorflowVariableDomain
from .compile.tensorflow import load_loss_function as load_loss_function
from .error import VehicleError as VehicleError
from .error import VehicleInternalError as VehicleInternalError
from .session.error import VehicleSessionClosed as VehicleSessionClosed
from .session.error import VehicleSessionUsed as VehicleSessionUsed
from .typing import AnyOptimiser as AnyOptimiser
from .typing import AnyOptimisers as AnyOptimisers
from .typing import DeclarationName as DeclarationName
from .typing import DifferentiableLogic as DifferentiableLogic
from .typing import Domain as Domain
from .typing import Domains as Domains
from .typing import Optimiser as Optimiser
from .typing import Optimisers as Optimisers
from .typing import QuantifiedVariableName as QuantifiedVariableName
from .typing import VariableDomain as VariableDomain

__all__: List[str] = [
    # Compile
    "load_loss_function",
    "VariableDomain",
    "TensorflowVariableDomain",
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
    "Domain",
    "Domains",
    "Optimiser",
    "Optimisers",
    "DifferentiableLogic",
]
