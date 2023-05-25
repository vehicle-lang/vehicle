import sys

from vehicle_lang._internal import _vehicle_cli


def main() -> None:
    exit(_vehicle_cli(sys.argv[1:]))


if __name__ == "__main__":
    main()
