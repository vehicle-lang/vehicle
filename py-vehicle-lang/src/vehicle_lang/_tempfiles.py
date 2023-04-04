import contextlib
from pathlib import Path
from tempfile import TemporaryDirectory as _TemporaryDirectory
from typing import Dict, Iterator, Optional, Tuple


class _TemporaryFile:
    path: Path

    def __init__(self, dir: _TemporaryDirectory, name: str, *, encoding: str = "utf8"):
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

    def cleanup(self):
        if self.path.exists():
            self.path.unlink()

    def __str__(self) -> str:
        return str(self.path)


@contextlib.contextmanager
def tempfiles(
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
