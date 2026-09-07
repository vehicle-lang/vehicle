import contextlib
import os
import sys
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import TYPE_CHECKING, Generator, Iterator, List, Optional, Sequence

from typing_extensions import TypeAlias

if TYPE_CHECKING or sys.version_info >= (3, 9):
    _StrTemporaryDirectory: TypeAlias = TemporaryDirectory[str]
else:
    _StrTemporaryDirectory: TypeAlias = TemporaryDirectory

__all__: List[str] = ["TemporaryFile", "temporary_files", "VEHICLE_PATH"]


# Needs to be kept up to date with `Vehicle.Prelude.IO.getVehiclePath`
VEHICLE_PATH = Path(
    os.environ.get(
        "VEHICLE_PATH",
        Path(
            os.environ.get(
                "APPDATA" if sys.platform == "win32" else "HOME", Path.home()
            )
        )
        / ".vehicle",
    )
)


class TemporaryFile:
    path: Path

    def __init__(
        self, dir: _StrTemporaryDirectory, name: str, *, encoding: str = "utf8"
    ):
        self.path = Path(dir.name) / name
        self.encoding = encoding

    def read_text(self) -> Optional[str]:
        if self.path.exists():
            contents = self.path.read_text(encoding=self.encoding).strip()
            if contents == "":
                return None
            else:
                return contents
        else:
            return None

    def cleanup(self) -> None:
        if self.path.exists():
            self.path.unlink()

    def __str__(self) -> str:
        return str(self.path)


@contextlib.contextmanager
def temporary_files(
    *names: str,
    prefix: Optional[str] = None,
    suffix: Optional[str] = None,
) -> Generator[Sequence[TemporaryFile], None, None]:
    dir = TemporaryDirectory(prefix=prefix, suffix=suffix)
    files: dict[str, TemporaryFile] = {}
    try:
        for name in names:
            files[name] = TemporaryFile(dir, name)
        yield tuple(files.values())
    finally:
        for name, file in files.items():
            try:
                file.cleanup()
            except Exception:
                pass  # Ignore cleanup errors
        try:
            dir.cleanup()
        except Exception:
            pass  # Ignore cleanup errors
