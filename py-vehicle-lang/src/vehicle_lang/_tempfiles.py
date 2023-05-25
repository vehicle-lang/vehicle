import contextlib
import sys
from pathlib import Path
from tempfile import TemporaryDirectory as _TemporaryDirectory
from typing import TYPE_CHECKING, Dict, Iterator, Optional, Tuple

from typing_extensions import TypeAlias

if TYPE_CHECKING or sys.version_info >= (3, 9):
    _StrTemporaryDirectory: TypeAlias = _TemporaryDirectory[str]
else:
    _StrTemporaryDirectory: TypeAlias = _TemporaryDirectory


class _TemporaryFile:
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
def _tempfiles(
    *names: str,
    prefix: Optional[str] = None,
    suffix: Optional[str] = None,
) -> Iterator[Tuple[_TemporaryFile, ...]]:
    try:
        dir = _TemporaryDirectory(prefix=prefix, suffix=suffix)
        tempfiles: Dict[str, _TemporaryFile] = {}
        for name in names:
            tempfiles[name] = _TemporaryFile(dir, name)
        yield tuple(tempfiles.values())
    finally:
        for name, tempfile in tempfiles.items():
            tempfile.cleanup()
        dir.cleanup()
