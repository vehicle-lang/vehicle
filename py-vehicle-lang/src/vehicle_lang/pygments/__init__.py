from ._external import ExternalLexer

__all__ = ["VehicleLexer"]


class VehicleLexer(ExternalLexer):
    name = "Vehicle"
    aliases = ["vehicle"]
    filenames = ["*.vcl"]
