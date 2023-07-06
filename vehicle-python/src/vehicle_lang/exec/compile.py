from pathlib import Path
from typing import Any, Dict, Optional, Union, cast

from .. import session as global_session
from ..session import Session
from .translation.python import PythonTranslation


def to_python(
    specification_path: Union[str, Path],
    *,
    declaration_context: Dict[str, Any] = {},
    session: Optional[Session] = None,
    translation: PythonTranslation,
) -> Dict[str, Any]:
    # Ensure that specification_path is a Path
    if isinstance(specification_path, str):
        specification_path = Path(specification_path)

    # Load the specification file
    if session is not None:
        program = session.load_program(specification_path)
    else:
        program = global_session.load_program(specification_path)

    # Translate the specification to a Python loss function:
    specification_filename = specification_path.name
    declaration_context = translation.compile(
        program, specification_filename, declaration_context
    )

    # Return the property, if it is declared:
    return declaration_context
