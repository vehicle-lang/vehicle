import sys

from vehicle_lang._internal import vehicleMain


def main():
    exit(vehicleMain(sys.argv[1:]))


if __name__ == "__main__":
    main()
