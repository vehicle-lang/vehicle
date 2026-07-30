import sys
from typing import Callable, Sequence

import vehicle_lang.session
from vehicle_lang.loss import codegen

# Python-side compile targets; everything else forwards to the Haskell binary.
_PYTHON_COMPILE_TARGETS: dict[str, Callable[[Sequence[str]], int]] = {
    "python-types": codegen.main,
}


def main() -> None:
    argv = sys.argv[1:]
    if len(argv) >= 2 and argv[0] == "compile" and argv[1] in _PYTHON_COMPILE_TARGETS:
        sys.exit(_PYTHON_COMPILE_TARGETS[argv[1]](argv[2:]))
    exit_code = vehicle_lang.session.check_call(argv)
    if len(argv) == 2 and argv[0] == "compile" and argv[1] in ("--help", "-h"):
        sys.stdout.flush()
        print(
            "  python-types             Compile @tensor record schemas to a typed Python module."
        )
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
