import contextlib
import sys
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import TYPE_CHECKING, Dict, Iterator, List, Optional, Sequence, Tuple

from typing_extensions import TypeAlias

if TYPE_CHECKING or sys.version_info >= (3, 9):
    _StrTemporaryDirectory: TypeAlias = TemporaryDirectory[str]
else:
    _StrTemporaryDirectory: TypeAlias = TemporaryDirectory

__all__: List[str] = ["TemporaryFile", "temporary_files"]


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
) -> Iterator[Sequence[TemporaryFile]]:
    try:
        dir = TemporaryDirectory(prefix=prefix, suffix=suffix)
        files: Dict[str, TemporaryFile] = {}
        for name in names:
            files[name] = TemporaryFile(dir, name)
        yield tuple(files.values())
    finally:
        for name, file in files.items():
            file.cleanup()
        dir.cleanup()
