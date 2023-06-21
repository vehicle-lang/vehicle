import sys

from .session import Session


def main() -> None:
    with Session() as session:
        exitCode = session.check_call(sys.argv[1:])
    exit(exitCode)


if __name__ == "__main__":
    main()
