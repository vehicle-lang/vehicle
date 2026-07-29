from pathlib import Path

from vehicle_lang.verify import verify

from ..config import PYTHON_TEST_SPECS_PATH, TEST_VERIFIER_PATH


def test_errors() -> None:
    """
    Tests that the Python bindings for verification successfully parses the event stream.
    """
    verify(
        specification=PYTHON_TEST_SPECS_PATH / "test_multiproperty.vcl",
        verifier="TestVerifier",
        verifier_location=TEST_VERIFIER_PATH,
        verifier_args=["unsat"],
        networks={"f": PYTHON_TEST_SPECS_PATH / "fake.onnx"},
    )
