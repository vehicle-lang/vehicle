import atexit
import sys
import tempfile
from contextlib import AbstractContextManager
from pathlib import Path
from types import TracebackType
from typing import TYPE_CHECKING, ClassVar, List, Optional, Sequence, Tuple, Type, Union

from typing_extensions import Self, TypeAlias

from ._binding import _unsafe_vehicle_free, _unsafe_vehicle_init, _unsafe_vehicle_main
from ._error import VehicleError as VehicleError
from ._error import VehicleSessionClosed as VehicleSessionClosed
from ._error import VehicleSessionUsed as VehicleSessionUsed
from ._target import Target
from ._temporary_files import temporary_files
from .ast import Program

__all__: List[str] = [
    "Target",
    "Session",
    "check_call",
    "check_output",
    "loads",
    "load",
]

if TYPE_CHECKING or sys.version_info >= (3, 9):
    SessionContextManager: TypeAlias = AbstractContextManager["Session"]
else:
    SessionContextManager: TypeAlias = AbstractContextManager


class Session(SessionContextManager):
    """
    The Session class enforces that the Haskell RTS is only initialised once,
    even when multiple calls to Vehicle are made.

    This is needed as initialising the Haskell RTS multiple times is unsafe,
    see: https://gitlab.haskell.org/ghc/ghc/-/issues/13693
    """

    _instance: ClassVar["Session"]
    _rts_init: bool
    _rts_exit: bool

    def __new__(cls) -> "Session":
        """
        This override of __new__ enforces that Session is a singleton, i.e.,
        that multiple calls to Session() return the same instance.
        """
        if not hasattr(cls, "_instance"):
            cls._instance = super(Session, cls).__new__(cls)
            cls._instance._rts_init = False
            cls._instance._rts_exit = False
        return cls._instance

    @property
    def closed(self) -> bool:
        return not self._rts_init or self._rts_exit

    def check_call(self, args: Sequence[str]) -> int:
        """
        Call Vehicle with the given arguments and return the exit code.
        """
        if not self.closed:
            return _unsafe_vehicle_main(args)
        else:
            raise VehicleSessionClosed()

    def check_output(
        self,
        args: Sequence[str],
    ) -> Tuple[int, Optional[str], Optional[str], Optional[str]]:
        """
        Call Vehicle with the given arguments and return the exit code, standard output, standard error, and logs.
        """
        with temporary_files("out", "err", "log", prefix="vehicle") as (out, err, log):
            exitCode = self.check_call(
                [
                    f"--redirect-stdout={out}",
                    f"--redirect-stderr={err}",
                    f"--redirect-logs={log}",
                    *args,
                ]
            )
            return (exitCode, out.read_text(), err.read_text(), log.read_text())

    def open(self, rts_args: Optional[Sequence[str]] = None) -> None:
        if self._rts_init:
            raise VehicleSessionUsed()
        else:
            self._rts_init = True
            _unsafe_vehicle_init(["vehicle", *(rts_args or [])])
            atexit.register(self.close)

    def close(self) -> None:
        if not self.closed:
            self._rts_exit = True
            _unsafe_vehicle_free()
            atexit.unregister(self.close)

    def __enter__(self) -> Self:
        if not self._rts_init:
            self.open()
        return self

    def __exit__(
        self,
        _exc_type: Optional[Type[BaseException]],
        _exc_value: Optional[BaseException],
        _exc_traceback: Optional[TracebackType],
    ) -> Optional[bool]:
        if not self.closed:
            self.close()
        return None

    def loads(self, spec: str, *, target: Target = Target.DEFAULT) -> Program:
        with tempfile.NamedTemporaryFile(mode="w") as loss_function_spec:
            loss_function_spec.write(spec)
            return self.load(loss_function_spec.name, target=target)

    def load(
        self, path: Union[str, Path], *, target: Target = Target.DEFAULT
    ) -> Program:
        exc, out, err, log = self.check_output(
            [
                "compile",
                "--target",
                target.vehicle_cli_name,
                "--json",
                "--specification",
                str(path),
            ]
        )
        if exc != 0:
            raise VehicleError(err or out or log or "unknown error")
        if out is None:
            raise VehicleError("no output")
        return Program.from_json(out)


def check_call(args: Sequence[str]) -> int:
    return Session().__enter__().check_call(args)


def check_output(
    args: Sequence[str],
) -> Tuple[int, Optional[str], Optional[str], Optional[str]]:
    return Session().__enter__().check_output(args)


def loads(spec: str, *, target: Target = Target.DEFAULT) -> Program:
    return Session().__enter__().load(spec, target=target)


def load(path: Union[str, Path], *, target: Target = Target.DEFAULT) -> Program:
    return Session().__enter__().load(path, target=target)
