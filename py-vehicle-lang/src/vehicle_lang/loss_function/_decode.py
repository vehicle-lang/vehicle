import collections.abc
from dataclasses import MISSING, dataclass, fields, is_dataclass
from typing import Any, Dict, Generic, List, Set, Tuple, Type, Union, cast

from typing_extensions import Literal, TypeAlias, TypeVar, get_args, get_origin

_T = TypeVar("_T")


JsonValue: TypeAlias = Union[
    str, bool, int, float, complex, List["JsonValue"], Dict[str, "JsonValue"]
]


def _cls_name(cls: Any) -> str:
    if isinstance(cls, type):
        return cls.__name__
    else:
        return str(cls)


@dataclass
class DecodeError(Exception):
    cls: Any
    value: JsonValue

    @property
    def value_cls_name(self) -> str:
        return _cls_name(type(self.value))

    def __str__(self) -> str:
        return f"Expected {_cls_name(self.cls)}, found {self.value_cls_name}"


@dataclass
class DecodeAsTupleError(DecodeError):
    cls_args: Tuple[Any, ...]

    def __str__(self) -> str:
        return f"Expected Tuple of length {len(self.cls_args)}, found {self.value_cls_name}"


@dataclass
class DecodeAsLiteralError(DecodeError):
    cls_args: Tuple[Any, ...]

    def __str__(self) -> str:
        return (
            f"Expected any of [{', '.join(self.cls_args)}], found {self.value_cls_name}"
        )


@dataclass
class DecodeAsUnionError(DecodeError):
    cls_args: Tuple[Any, ...]

    def __str__(self) -> str:
        return f"Expected value of type {' | '.join(self.cls_args)}, found {self.value_cls_name}"


@dataclass
class DecodeAsDataclassError(DecodeError, Generic[_T]):
    exc: Dict[Type[_T], Exception]

    def __str__(self) -> str:
        return "\n".join(
            [f"{_cls_name(cls_alt)} -> {e}" for cls_alt, e in self.exc.items()]
        )


@dataclass
class UnusedFields(DecodeError):
    fld_names: Set[str]

    def __str__(self) -> str:
        return f"Unused fields {', '.join(self.fld_names)}"


@dataclass
class MissingFields(DecodeError):
    fld_names: Set[str]

    def __str__(self) -> str:
        return f"Missing fields {', '.join(self.fld_names)}"


def decode(cls: Union[Type[_T], Any], value: JsonValue) -> _T:
    # str
    if cls is str:
        if not isinstance(value, str):
            raise DecodeError(cls, value)
        return cast(_T, value)

    # int
    if cls is int:
        if not isinstance(value, int):
            raise DecodeError(cls, value)
        return cast(_T, value)

    # float
    if cls is float:
        if not isinstance(value, (int, float)):
            raise DecodeError(cls, value)
        return cast(_T, float(value))

    # complex
    if cls is complex:
        if not isinstance(value, (int, float, complex)):
            raise DecodeError(cls, value)
        return cast(_T, complex(value))

    # tuple
    if get_origin(cls) in (tuple,):
        if not isinstance(value, List):
            raise DecodeError(cls, value)
        cls_args = get_args(cls)

        # tuple[()]
        if len(cls_args) == 1 and cls_args[0] is ():
            if not value:
                return cast(_T, ())
            else:
                raise DecodeAsTupleError(cls, value, cls_args)

        # tuple[_T, ...]
        if len(cls_args) == 2 and cls_args[1] is ...:
            cls_item = cls_args[0]
            return cast(_T, [decode(cls_item, item) for item in value])

        # tuple
        if len(cls_args) != len(value):
            raise DecodeAsTupleError(cls, value, cls_args)
        return cast(
            _T,
            tuple((decode(cls_item, item) for cls_item, item in zip(cls_args, value))),
        )

    # list, Sequence, MutableSequence
    if get_origin(cls) in (
        list,
        collections.abc.Sequence,
        collections.abc.MutableSequence,
    ):
        if not isinstance(value, List):
            raise DecodeError(cls, value)
        try:
            cls_item, *_ = get_args(cls)
        except ValueError:
            raise TypeError(f"{cls.__name__} requires a type annotation")
        return cast(_T, [decode(cls_item, item) for item in value])

    # dict, Mapping, MutableMapping
    if get_origin(cls) in (
        dict,
        collections.abc.Mapping,
        collections.abc.MutableMapping,
    ):
        if not isinstance(value, Dict):
            raise DecodeError(cls, value)
        try:
            cls_key, cls_val, *_ = get_args(cls)
        except ValueError:
            raise TypeError(f"{cls.__name__} requires a type annotation")
        return cast(
            _T,
            {decode(cls_key, key): decode(cls_val, val) for key, val in value.items()},
        )

    # Literal
    if get_origin(cls) is Literal:
        cls_args = get_args(cls)
        if value in cls_args:
            return cast(_T, value)
        else:
            raise DecodeAsLiteralError(cls, value, cls_args)

    # Union
    if get_origin(cls) is Union:
        cls_args = get_args(cls)
        for cls_alt in cls_args:
            try:
                return decode(cls_alt, value)
            except DecodeError:
                continue
        raise DecodeAsUnionError(cls, value, cls_args)

    # Dataclass
    if is_dataclass(cls):
        if not isinstance(value, Dict):
            raise DecodeError(cls, value)

        # Try all subclasses of cls:
        exc_buffer: Dict[Type[_T], Exception] = {}
        for cls_alt in (cls, *cls.__subclasses__()):
            try:
                # Decode each field that is marked as init:
                kwargs: Dict[str, Any] = {}
                for fld in fields(cls_alt):
                    if fld.init and fld.name in value:
                        kwargs[fld.name] = decode(fld.type, value[fld.name])

                # Check for superfluous arguments:
                unused_fld_names: Set[str] = set(kwargs.keys())
                unused_fld_names.difference_update(
                    (fld.name for fld in fields(cls_alt))
                )
                if unused_fld_names:
                    raise UnusedFields(cls_alt, value, unused_fld_names)

                # Check for missing arguments:
                missing_fld_names: Set[str] = set()
                for fld in fields(cls_alt):
                    if fld.init and fld.name not in kwargs:
                        if fld.default is not MISSING:
                            kwargs[fld.name] = fld.default
                        elif fld.default_factory is not MISSING:
                            kwargs[fld.name] = fld.default_factory()
                        else:
                            missing_fld_names.add(fld.name)
                if missing_fld_names:
                    raise MissingFields(cls_alt, value, missing_fld_names)

                return cast(_T, cls_alt(**kwargs))
            except (DecodeError, TypeError) as e:
                exc_buffer[cls_alt] = e
                continue

        raise DecodeAsDataclassError(cls, value, exc_buffer)

    raise DecodeError(cls, value)
