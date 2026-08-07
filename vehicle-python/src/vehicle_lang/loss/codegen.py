"""Emit a typed Python module from a spec's @tensor record schemas."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Sequence, TextIO

from .. import session
from .._ast import _nodes as vcl
from ..error import VehicleInternalError
from ._records import render_module, schemas_of


def generate(spec_path: str | Path, output: TextIO | Path) -> None:
    """Read 'spec_path' and emit torch.Tensor subclasses for each @tensor record schema."""
    raw = session.execute_command(
        ["--json", "list", "records", f"--specification={spec_path}"]
    )
    if raw is None:
        raise VehicleInternalError("vehicle list records produced no output")
    text = render_module(schemas_of(vcl.Program.from_json(raw)), Path(spec_path).name)

    if isinstance(output, (str, Path)):
        Path(output).write_text(text)
    else:
        output.write(text)


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="vehicle compile python-types",
        description="Emit a typed Python module with torch.Tensor subclasses for each @tensor record in the spec.",
    )
    parser.add_argument(
        "-s", "--specification", required=True, help="path to the .vcl spec"
    )
    parser.add_argument(
        "-o",
        "--output",
        required=True,
        help="path to the output .py file, or '-' for stdout",
    )
    args = parser.parse_args(argv)

    if args.output == "-":
        generate(args.specification, sys.stdout)
    else:
        generate(args.specification, Path(args.output))
    return 0


if __name__ == "__main__":
    sys.exit(main())
