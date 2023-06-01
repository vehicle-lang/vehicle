import sys

if sys.version_info >= (3, 8):
    from functools import singledispatchmethod as singledispatchmethod
else:
    from types import MappingProxyType
    from typing import Any, Callable, Generic, Type, overload

    from typing_extensions import TypeVar

    _T = TypeVar("_T")
    _S = TypeVar("_S")

    class _SingleDispatchCallable(Generic[_T]):
        registry: MappingProxyType[Any, Callable[..., _T]]
        def dispatch(self, cls: Any) -> Callable[..., _T]: ...
        # @fun.register(complex)
        # def _(arg, verbose=False): ...
        @overload
        def register(
            self, cls: Type[Any], func: None = None
        ) -> Callable[[Callable[..., _T]], Callable[..., _T]]: ...
        # @fun.register
        # def _(arg: int, verbose=False):
        @overload
        def register(
            self, cls: Callable[..., _T], func: None = None
        ) -> Callable[..., _T]: ...
        # fun.register(int, lambda x: x)
        @overload
        def register(
            self, cls: Type[Any], func: Callable[..., _T]
        ) -> Callable[..., _T]: ...
        def _clear_cache(self) -> None: ...
        def __call__(__self, *args: Any, **kwargs: Any) -> _T: ...

    class singledispatchmethod(Generic[_T]):
        dispatcher: _SingleDispatchCallable[_T]
        func: Callable[..., _T]
        def __init__(self, func: Callable[..., _T]) -> None: ...
        @property
        def __isabstractmethod__(self) -> bool: ...
        @overload
        def register(
            self, cls: Type[Any], method: None = None
        ) -> Callable[[Callable[..., _T]], Callable[..., _T]]: ...
        @overload
        def register(
            self, cls: Callable[..., _T], method: None = None
        ) -> Callable[..., _T]: ...
        @overload
        def register(
            self, cls: Type[Any], method: Callable[..., _T]
        ) -> Callable[..., _T]: ...
        def __get__(
            self, obj: _S, cls: Type[_S] | None = None
        ) -> Callable[..., _T]: ...
