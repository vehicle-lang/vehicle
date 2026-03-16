import atexit
import io
import os
import pty
import sys
from contextlib import AbstractContextManager, redirect_stderr, redirect_stdout
from types import TracebackType
from typing import ClassVar, Optional, Sequence, Type

from typing_extensions import Self

from .._binding import _unsafe_vehicle_free, _unsafe_vehicle_init, _unsafe_vehicle_main
from .._temporary_files import temporary_files
from .error import VehicleSessionClosed, VehicleSessionUsed


class SessionContextManager(AbstractContextManager["Session"]):
    """
    An abstract base class for Vehicle sessions that implement context management.
    """


class Session(SessionContextManager):
    """
    The Session class enforces that the Haskell RTS is only initialised once,
    even when multiple calls to Vehicle are made.

    This is needed as initialising the Haskell RTS multiple times is unsafe,
    see: https://gitlab.haskell.org/ghc/ghc/-/issues/13693
    """

    _instance: ClassVar[Self]
    _rts_init: bool
    _rts_exit: bool

    def __new__(cls: Type[Self]) -> "Session":
        """
        This override of __new__ enforces that Session is a singleton, i.e.,
        that multiple calls to Session() return the same instance.
        """
        if not hasattr(cls, "_instance"):
            cls._instance = super(Session, cls).__new__(cls)
            cls._instance._rts_init = False
            cls._instance._rts_exit = False
        return cls._instance

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

    def check_call(self, args: Sequence[str]) -> int:
        if not self.closed:
            return _unsafe_vehicle_main(args)
        else:
            raise VehicleSessionClosed()

    def check_output(
        self,
        args: Sequence[str],
    ) -> tuple[int, Optional[str], Optional[str], Optional[str]]:
        with redirect_stdout(io.StringIO()) as out:
            with redirect_stderr(io.StringIO()) as err:
                with temporary_files("log", prefix="vehicle") as (log,):
                    exitCode = self.check_call(
                        [
                            f"--redirect-logs={log}",
                            *args,
                        ]
                    )
                    return (
                        exitCode,
                        out.getvalue() or None,
                        err.getvalue() or None,
                        log.read_text(),
                    )

    def check_output_pty(
        self, args: Sequence[str]
    ) -> tuple[int, Optional[str], Optional[str], Optional[str]]:
        import select
        import threading

        stdout_fd = sys.stdout.fileno()
        stderr_fd = sys.stderr.fileno()

        # Create a pseudo-terminal pair to capture stdout
        provider_fd, receiver_fd = pty.openpty()
        # Create a pipe to capture stderr
        pread_fd, pwrite_fd = os.pipe()

        saved_stdout_fd = os.dup(stdout_fd)
        saved_stderr_fd = os.dup(stderr_fd)

        # Storage for captured output
        out_lines: list[str] = []
        read_exception: list[Exception] = []

        def read_pty_output() -> None:
            """Read from PTY in non-blocking mode while process runs."""
            try:
                # Set provider_fd to non-blocking mode
                import fcntl

                flags = fcntl.fcntl(provider_fd, fcntl.F_GETFL)
                fcntl.fcntl(provider_fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)

                with os.fdopen(provider_fd, "r", buffering=1) as f:
                    while True:
                        # Use select to wait for data with timeout
                        ready, _, _ = select.select([f], [], [], 0.1)
                        if ready:
                            try:
                                line = f.readline()
                                if not line:
                                    break
                                out_lines.append(line)
                            except OSError:
                                break
                        # Check if process has finished and no more data
                        elif hasattr(threading.current_thread(), "_stop_reading"):
                            # Do one final non-blocking read attempt
                            try:
                                remaining = f.read()
                                if remaining:
                                    out_lines.append(remaining)
                            except (OSError, ValueError):
                                pass
                            break
            except Exception as e:
                read_exception.append(e)

        # Start reading thread
        reader_thread = threading.Thread(target=read_pty_output, daemon=True)
        reader_thread.start()

        try:
            # Redirect stdout and stderr
            os.dup2(receiver_fd, stdout_fd)
            os.dup2(pwrite_fd, stderr_fd)

            # Close unused write ends
            os.close(receiver_fd)
            os.close(pwrite_fd)

            with temporary_files("log", prefix="vehicle") as (log,):
                exitCode = self.check_call([f"--redirect-logs={log}", *args])

            sys.stdout.flush()

        finally:
            # Signal reader thread to finish
            reader_thread._stop_reading = True  # type: ignore

            # Restore stdout and stderr
            os.dup2(saved_stdout_fd, stdout_fd)
            os.dup2(saved_stderr_fd, stderr_fd)

            # Cleanup
            os.close(saved_stdout_fd)
            os.close(saved_stderr_fd)

        # Wait for reader thread to finish
        reader_thread.join(timeout=5.0)

        # Check if reading thread encountered an error
        if read_exception:
            raise read_exception[0]

        with os.fdopen(pread_fd, "r") as f:
            err = f.read()

        return (
            exitCode,
            "".join(out_lines) or None,
            err or None,
            log.read_text(),
        )

    def close(self) -> None:
        if not self.closed:
            self._rts_exit = True
            _unsafe_vehicle_free()
            atexit.unregister(self.close)

    @property
    def closed(self) -> bool:
        return not self._rts_init or self._rts_exit

    def open(self, rts_args: Optional[Sequence[str]] = None) -> None:
        if self._rts_init:
            raise VehicleSessionUsed()
        else:
            self._rts_init = True
            _unsafe_vehicle_init(["vehicle", *(rts_args or [])])
            atexit.register(self.close)
