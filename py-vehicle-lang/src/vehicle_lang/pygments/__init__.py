try:
    from .external import ExternalLexer

    __all__ = ["VehicleLexer"]

    class VehicleLexer(ExternalLexer):
        name = "Vehicle"
        aliases = ["vehicle"]
        filenames = ["*.vcl"]

except ImportError:
    import sys

    sys.stderr.write("To use the VehicleLexer, install vehicle_lang[pygments].")
