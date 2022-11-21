from typing import NoReturn


def internal_error_msg(msg) -> NoReturn:
    raise Exception(
        "This is an internal error. Please raise an issue at https://github.com/vehicle-lang/vehicle/issues with the following error message: "
        + msg
    )
