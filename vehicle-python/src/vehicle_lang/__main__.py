import sys

import vehicle_lang.session

# Python-side compile targets; everything else forwards to the Haskell binary.
_PYTHON_COMPILE_TARGETS: dict[str, str] = {
    "python-types": "vehicle_lang.loss.codegen",
}


def main() -> None:
    argv = sys.argv[1:]
    if len(argv) >= 2 and argv[0] == "compile" and argv[1] in _PYTHON_COMPILE_TARGETS:
        sys.exit(_dispatch(argv[1], argv[2:]))
    exit_code = vehicle_lang.session.check_call(argv)
    if len(argv) == 2 and argv[0] == "compile" and argv[1] in ("--help", "-h"):
        sys.stdout.flush()
        print(
            "  python-types             Compile @tensor record schemas to a typed Python module."
        )
    sys.exit(exit_code)


def _dispatch(target: str, rest: list[str]) -> int:
    import importlib

    module = importlib.import_module(_PYTHON_COMPILE_TARGETS[target])
    sys.argv = [f"vehicle compile {target}", *rest]
    return int(module.main())


if __name__ == "__main__":
    main()
