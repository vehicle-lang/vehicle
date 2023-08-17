import sys

import vehicle_lang.session


def main() -> None:
    exit(vehicle_lang.session.check_call(sys.argv[1:]))


if __name__ == "__main__":
    main()
