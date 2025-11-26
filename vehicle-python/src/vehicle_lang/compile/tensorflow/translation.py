import ast as py
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Iterable, Mapping, Optional, cast

import tensorflow as tf

from ..ast import load as ast_load
from ...typing import (
    DeclarationName,
    DifferentiableLogic,
    Explicit,
    Target,
)
from ..error import (
    VehiclePropertyNotCallable,
    VehiclePropertyNotFound,
)
from ..python import PythonTranslation
from .builtins import TensorFlowBuiltins
from .samplers import TensorFlowSampler

# Create proper Python AST provenance (different from Vehicle provenance)
PY_MISSING = {"lineno": 0, "col_offset": 0}

################################################################################
### TensorFlow Translation
################################################################################


@dataclass(frozen=True, init=False)
class TensorFlowTranslation(PythonTranslation):
    def __init__(self) -> None:
        super().__init__(
            builtins=TensorFlowBuiltins(),
            module_header=[
                py.Import(
                    names=[
                        py.alias(
                            name="tensorflow",
                            asname=None,
                            lineno=0,
                            col_offset=0,
                        )
                    ],
                    lineno=0,
                    col_offset=0,
                )
            ],
        )


def load(
    path: str | Path,
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = Explicit.Explicit,
    samplers: Mapping[str, TensorFlowSampler],
    translation: Optional[TensorFlowTranslation] = None,
) -> dict[str, Any]:
    if translation is None:
        translation = TensorFlowTranslation()
    return translation.compile(
        ast_load(path, declarations=declarations, target=target),
        path=path,
        declaration_context={},
        samplers=samplers,
    )


def load_loss_function(
    path: str | Path,
    property_name: DeclarationName,
    *,
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
    samplers: Mapping[str, TensorFlowSampler] = {},
) -> Callable[..., tf.Tensor]:
    """
    Load a loss function from a property in a Vehicle specification.

    :param path: The path to the Vehicle specification file.
    :param property_name: The name of the Vehicle property to load.
    :param target: The differentiable logic to use for interpreting the Vehicle property as a loss function, defaults to the Vehicle logic.
    :param samplers: A map from quantified variable names to samplers for their values. See `ABCSampler` for more details.
    :return: A function that takes the required external resources in the specification as keyword arguments and returns the loss corresponding to the property.
    """
    if not samplers:
        samplers = {}  # TODO: provide default samplers
    declarations = load(
        path, declarations=(property_name,), samplers=samplers, target=target
    )
    if property_name in declarations:
        property_func = declarations[property_name]
        if callable(property_func):
            return cast(Callable[..., tf.Tensor], property_func)
        else:
            raise VehiclePropertyNotCallable(property_name)
    else:
        raise VehiclePropertyNotFound(property_name)