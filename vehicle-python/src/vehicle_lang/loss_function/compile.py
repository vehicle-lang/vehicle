from pathlib import Path
from typing import Any, Callable, Dict, Optional, Union, cast

from typing_extensions import TypeAlias, override

from .. import session as global_session
from ..session import Session
from .translation.python import PythonTranslation
from .translation.tensorflow import TensorflowTranslation

LossFunction: TypeAlias = Callable[[], float]


def to_python(
    specification_path: Union[str, Path],
    property_name: str,
    *,
    declaration_context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    translation: PythonTranslation = PythonTranslation(),
) -> LossFunction:
    # Ensure that specification_path is a Path
    if isinstance(specification_path, str):
        specification_path = Path(specification_path)

    # Load the specification file
    if session is not None:
        module = session.load(specification_path)
    else:
        module = global_session.load(specification_path)

    # Translate the specification to a Python loss function:
    specification_filename = specification_path.name
    declaration_context = translation.compile(
        module, specification_filename, declaration_context
    )

    # Return the property, if it is declared:
    if property_name in declaration_context:
        return cast(LossFunction, declaration_context[property_name])
    else:
        raise ValueError(
            f"Could not find property {property_name} in {specification_filename}"
        )


def to_tensorflow(
    specification_path: Union[str, Path],
    property_name: str,
    *,
    declaration_context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    translation: TensorflowTranslation = TensorflowTranslation(),
) -> LossFunction:
    return to_python(
        specification_path,
        property_name,
        declaration_context=declaration_context,
        session=session,
        translation=translation,
    )
