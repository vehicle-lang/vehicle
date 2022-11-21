def internal_error_msg(msg):
    raise Exception(
        "This is an internal error. Please raise an issue at https://github.com/vehicle-lang/vehicle/issues with the following error message: "
        + msg
    )
