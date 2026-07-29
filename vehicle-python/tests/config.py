from pathlib import Path

HASKELL_TESTS_PATH = Path(__file__).parent.parent.parent / "vehicle" / "tests"

HASKELL_GOLDEN_TESTS_PATH = HASKELL_TESTS_PATH / "golden"

TEST_VERIFIER_PATH = HASKELL_TESTS_PATH / "testVerifier"

PYTHON_TEST_SPECS_PATH = Path(__file__).parent / "data"
