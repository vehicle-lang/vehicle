import json
from typing import Any, Dict, Optional


class VehicleError(Exception):
    """Base class for all Vehicle errors."""

    def __init__(self, message: str):
        self.message = message
        super().__init__(message)

    @classmethod
    def from_json(cls, json_str: str) -> "VehicleError":
        """Create a Vehicle error from a JSON string."""
        try:
            data = json.loads(json_str)
            if isinstance(data, dict) and "error" in data:
                return cls(data["error"])
            return cls(json_str)
        except json.JSONDecodeError:
            return cls(json_str)


class VehicleInternalError(VehicleError):
    """Error raised when an internal error occurs in Vehicle."""

    pass


class VehicleSyntaxError(VehicleError):
    """Error raised when a syntax error occurs in a Vehicle specification."""

    def __init__(
        self,
        message: str,
        file: Optional[str] = None,
        line: Optional[int] = None,
        column: Optional[int] = None,
    ):
        self.file = file
        self.line = line
        self.column = column
        super().__init__(message)

    @classmethod
    def from_json(cls, json_str: str) -> "VehicleSyntaxError":
        """Create a VehicleSyntaxError from a JSON string."""
        try:
            data = json.loads(json_str)
            if isinstance(data, dict):
                if "error" in data:
                    return cls(data["error"])

                message = data.get("problem", "Syntax error")
                file = None
                line = None
                column = None

                if "provenance" in data:
                    provenance = data["provenance"]
                    if isinstance(provenance, dict):
                        file = provenance.get("file")
                        if "range" in provenance:
                            range_data = provenance["range"]
                            if isinstance(range_data, dict) and "start" in range_data:
                                start = range_data["start"]
                                if isinstance(start, dict):
                                    line = start.get("line")
                                    column = start.get("column")

                return cls(message, file, line, column)

            return cls(json_str)
        except json.JSONDecodeError:
            return cls(json_str)

    def __str__(self) -> str:
        parts = [self.message]
        if self.file:
            parts.append(f"in file: {self.file}")
        if self.line is not None:
            parts.append(f"at line: {self.line}")
        if self.column is not None:
            parts.append(f"column: {self.column}")

        return ", ".join(parts)
