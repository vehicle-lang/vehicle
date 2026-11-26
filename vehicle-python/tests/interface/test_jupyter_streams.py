"""Regression test for https://github.com/vehicle-lang/vehicle/issues/1173.

Jupyter replaces `sys.stdout` and `sys.stderr` with stream objects that forward their writes to the
notebook frontend, while `fileno()` still reports some unrelated OS-level file descriptor. Output
captured at the file-descriptor level therefore misses everything Vehicle writes, since the
bindings write through `sys.stdout`. In that case `load_ast` raised "no output" and the compiled
program was dumped into the notebook. The bindings must capture output through `sys.stdout`
itself.
"""

import io
import os
import sys
from contextlib import ExitStack

from vehicle_lang.loss import load_ast

from ..config import PYTHON_TEST_SPECS_PATH


class NotebookLikeStream(io.StringIO):
    """A stream whose writes do not go to the file descriptor reported by `fileno`."""

    def __init__(self, fd: int) -> None:
        super().__init__()
        self._fd = fd

    def fileno(self) -> int:
        return self._fd


def test_load_ast_with_notebook_streams() -> None:
    # The streams are swapped inside the test body rather than in a fixture, because pytest's own
    # output capturing reassigns `sys.stdout` and `sys.stderr` between fixture setup and the test.
    with ExitStack() as stack:
        stdout_fd = os.open(os.devnull, os.O_WRONLY)
        stack.callback(os.close, stdout_fd)
        stderr_fd = os.open(os.devnull, os.O_WRONLY)
        stack.callback(os.close, stderr_fd)

        stdout = NotebookLikeStream(stdout_fd)
        stderr = NotebookLikeStream(stderr_fd)
        saved_stdout, saved_stderr = sys.stdout, sys.stderr
        stack.callback(setattr, sys, "stdout", saved_stdout)
        stack.callback(setattr, sys, "stderr", saved_stderr)
        sys.stdout, sys.stderr = stdout, stderr

        program = load_ast(PYTHON_TEST_SPECS_PATH / "test_addition.vcl")

    assert program is not None
    # The compiled program must be captured, not leaked to the notebook.
    assert stdout.getvalue() == ""
