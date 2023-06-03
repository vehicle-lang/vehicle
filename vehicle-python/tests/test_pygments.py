import difflib
import os
import subprocess
import tempfile
from pathlib import Path

import pytest

PACKAGE_ROOT = Path(__file__).parent.parent
VEHICLE_GOLDEN_TESTS_PATH = PACKAGE_ROOT / "vendor" / "vehicle" / "tests" / "golden"
VEHICLE_GOLDEN_TOKENS_PATH = PACKAGE_ROOT / "tests" / "golden"
VEHICLE_FILES = VEHICLE_GOLDEN_TESTS_PATH.glob("**/*.vcl")


@pytest.mark.parametrize(
    "vehicle_file",
    VEHICLE_FILES,
)  # type: ignore[misc]
def test_pygments(vehicle_file: Path) -> None:
    UPDATE_GOLDENS = os.environ["UPDATE_GOLDENS"]
    temporary_file = tempfile.NamedTemporaryFile("r")
    subprocess.check_call(
        [
            "pygmentize",
            "-l",
            "vehicle",
            "-f",
            "tokens",
            str(vehicle_file),
            "-o",
            temporary_file.name,
        ]
    )
    vehicle_golden_tokens_file = VEHICLE_GOLDEN_TOKENS_PATH / vehicle_file.relative_to(
        VEHICLE_GOLDEN_TESTS_PATH
    ).with_suffix(".vcl.tokens.golden")

    if UPDATE_GOLDENS:
        vehicle_golden_tokens_file.parent.mkdir(parents=True, exist_ok=True)
        vehicle_golden_tokens_file.write_text(temporary_file.read())
    elif vehicle_golden_tokens_file.exists():
        with vehicle_golden_tokens_file.open("r") as golden_file:
            diff = list(
                difflib.unified_diff(
                    temporary_file.readlines(),
                    golden_file.readlines(),
                    fromfile=temporary_file.name,
                    tofile=str(vehicle_golden_tokens_file),
                )
            )
            if diff:
                raise ValueError("\n".join(diff))
    else:
        raise FileNotFoundError(str(vehicle_golden_tokens_file))
