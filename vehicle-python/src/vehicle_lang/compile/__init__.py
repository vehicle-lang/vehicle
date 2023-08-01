from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Union

from ..ast import (
    AST,
    Binder,
    BuiltinFunction,
    Declaration,
    Expression,
    Program,
    Provenance,
    load,
)
from ..error import VehiclePropertyNotFound
from ..typing import AnySamplers, DeclarationName, DifferentiableLogic, Explicit, Target
from .abc import AnyBuiltins
from .python import PythonBuiltins, PythonTranslation

__all__: List[str] = [
    # Abstract Syntax Tree
    "AST",
    "Binder",
    "BuiltinFunction",
    "Declaration",
    "Expression",
    "Program",
    "Provenance",
    # Translation to Python
    "PythonBuiltins",
    "PythonTranslation",
    # High-level functions
    "compile",
    "load_loss_function",
]


def compile(
    path: Union[str, Path],
    *,
    declarations: Iterable[DeclarationName] = (),
    target: Target = Explicit.Explicit,
    translation: Optional[PythonTranslation] = None,
) -> Dict[str, Any]:
    if translation is None:
        translation = PythonTranslation(builtins=PythonBuiltins(samplers={}))
    return translation.compile(
        load(path, declarations=declarations, target=target), path=path
    )


def load_loss_function(
    path: Union[str, Path],
    property_name: DeclarationName,
    *,
    target: DifferentiableLogic = DifferentiableLogic.Vehicle,
    samplers: AnySamplers = {},
) -> Any:
    """
    Load a loss function from a property in a Vehicle specification.

    :param path: The path to the Vehicle specification file.
    :param property_name: The name of the Vehicle property to load.
    :param target: The differentiable logic to use for interpreting the Vehicle property as a loss function, defaults to the Vehicle logic.
    :param samplers: A map from quantified variable names to samplers for their values. See `Sampler` for more details.
    :return: A function that takes the required external resources in the specification as keyword arguments and returns the loss corresponding to the property.
    """
    translation = PythonTranslation(builtins=PythonBuiltins(samplers=samplers))
    declarations = compile(
        path,
        declarations=(property_name,),
        target=target,
        translation=translation,
    )
    if property_name in declarations:
        return declarations[property_name]
    else:
        raise VehiclePropertyNotFound(property_name)
