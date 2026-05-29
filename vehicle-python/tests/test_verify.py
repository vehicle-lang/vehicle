from pathlib import Path

import pytest
from vehicle_lang.error import VehicleUserError
from vehicle_lang.list import list_entities
from vehicle_lang.verify import verify

GOLDEN_PATH = Path(__file__).parent / "data"
TEST_VERIFIER_PATH = (
    Path(__file__).parent.parent.parent / "vehicle" / "tests" / "testVerifier"
)


def test_errors() -> None:
    verify(
        specification=GOLDEN_PATH / "test_multiproperty.vcl",
        verifier="TestVerifier",
        verifier_location=TEST_VERIFIER_PATH,
        verifier_args=["unsat"],
        networks={"f": GOLDEN_PATH / "fake.onnx"},
    )
