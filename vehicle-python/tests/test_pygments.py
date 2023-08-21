import difflib
import os
import subprocess
from pathlib import Path

import pytest

TESTS_DATA = Path(__file__).parent / "data"


@pytest.mark.parametrize(
    "vehicle_file",
    TESTS_DATA.glob("*.vcl"),
)  # type: ignore[misc]
def test_pygments(vehicle_file: Path) -> None:
    UPDATE_GOLDENS = os.environ.get("UPDATE_GOLDENS")
    actual_tokens = subprocess.check_output(
        [
            "pygmentize",
            "-l",
            "vehicle",
            "-f",
            "tokens",
            str(vehicle_file),
        ],
        encoding="utf-8",
    )
    vehicle_golden_tokens_file = vehicle_file.with_suffix(".vcl.tokens.golden")

    if UPDATE_GOLDENS:
        vehicle_golden_tokens_file.parent.mkdir(parents=True, exist_ok=True)
        vehicle_golden_tokens_file.write_text(actual_tokens)
    elif vehicle_golden_tokens_file.exists():
        with vehicle_golden_tokens_file.open("r") as golden_file:
            diff = list(
                difflib.unified_diff(
                    actual_tokens.splitlines(keepends=True),
                    golden_file.readlines(),
                    fromfile=str(vehicle_golden_tokens_file.with_suffix(".actual")),
                    tofile=str(vehicle_golden_tokens_file),
                )
            )
            if diff:
                pytest.fail(reason="\n".join(diff))
    else:
        raise FileNotFoundError(str(vehicle_golden_tokens_file))
