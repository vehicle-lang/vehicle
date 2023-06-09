import random
from pathlib import Path
from typing import Any, Dict

import pytest
from vehicle_lang import session
from vehicle_lang.loss_function.translation.tensorflow import TensorflowTranslation


def sampler_for_x(**ctx: Any) -> float:
    return random.uniform(0.5, 0.5)


@pytest.mark.parametrize(
    "specification_filename,network_name,input_declaration_context",
    (
        (
            "test_bounded.vcl",
            "f",
            {"sampler_for_x": sampler_for_x},
        ),
    ),
)  # type: ignore[misc]
def test_loss_function_tensorflow(
    specification_filename: str,
    network_name: str,
    input_declaration_context: Dict[str, Any],
) -> None:
    try:
        specification_path = Path(__file__).parent / "data" / specification_filename
        module = session.load(specification_path)
        compiler = TensorflowTranslation()
        network_context = {network_name: lambda x: x}
        output_declaration_context = compiler.compile(
            module,
            specification_filename,
            {**input_declaration_context, **network_context},
        )

    except ModuleNotFoundError:
        from logging import warning

        warning("test_loss_function_tensorflow requires tensorflow")
