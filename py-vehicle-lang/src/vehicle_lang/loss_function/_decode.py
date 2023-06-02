import collections.abc
from dataclasses import MISSING, dataclass, fields, is_dataclass
from typing import (
    Any,
    Dict,
    Iterator,
    List,
    Optional,
    Sequence,
    Tuple,
    Type,
    Union,
    cast,
)

from typing_extensions import Literal, TypeAlias, TypeVar, get_args, get_origin

_T = TypeVar("_T")


JsonValue: TypeAlias = Union[
    str, bool, int, float, complex, List["JsonValue"], Dict[str, "JsonValue"]
]

_TYPE: str = "tag"
_ARGS: str = "contents"


def _type_name(cls: Any) -> str:
    if isinstance(cls, type):
        return cls.__name__
    else:
        return str(cls)


def _subcls_iter(cls: Type[_T]) -> Iterator[Type[_T]]:
    """
    Iterate over a class and its subclasses.
    """
    yield cls
    yield from cls.__subclasses__()


def _subcls_find(cls: Type[_T], cls_name: str) -> Optional[Type[_T]]:
    """
    Find a subclass by name.
    """
    for subcls in _subcls_iter(cls):
        if subcls.__name__ == cls_name:
            return subcls
    return None


@dataclass
class DecodeError(Exception):
    value: JsonValue
    cls: Any
    reason: Optional[str] = None

    def __str__(self) -> str:
        expected_type_str = _type_name(self.cls)
        found_type_str = _type_name(type(self.value))
        return "".join(
            [
                f"Could not decode value of type {found_type_str} as {expected_type_str}",
                f": {self.reason}." if self.reason else ".",
                f"\n{repr(self.value)}",
            ]
        )


@dataclass
class DecodeFieldError(Exception):
    value: JsonValue
    telescope: Sequence[Tuple[Type[Any], str]]
    reason: DecodeError

    def __str__(self) -> str:
        telescope_str = " > ".join(
            [f"{_type_name(cls)}.{fld}" for cls, fld in self.telescope]
        )
        return f"Could not decode {telescope_str}: {self.reason}"


def decode(cls: Union[Type[_T], Any], value: JsonValue) -> _T:
    # str
    if cls is str:
        if not isinstance(value, str):
            raise DecodeError(value, cls)
        return cast(_T, value)

    # int
    if cls is int:
        if not isinstance(value, int):
            raise DecodeError(value, cls)
        return cast(_T, value)

    # float
    if cls is float:
        if not isinstance(value, (int, float)):
            raise DecodeError(value, cls)
        return cast(_T, float(value))

    # complex
    if cls is complex:
        if not isinstance(value, (int, float, complex)):
            raise DecodeError(value, cls)
        return cast(_T, complex(value))

    # tuple
    if get_origin(cls) in (tuple,):
        if not isinstance(value, List):
            raise DecodeError(value, cls)
        cls_args = get_args(cls)

        # tuple[()]
        if len(cls_args) == 1 and cls_args[0] is ():
            if not value:
                return cast(_T, ())
            else:
                raise DecodeError(value, cls, "mismatched length")

        # tuple[_T, ...]
        if len(cls_args) == 2 and cls_args[1] is ...:
            cls_item = cls_args[0]
            return cast(_T, [decode(cls_item, item) for item in value])

        # tuple
        if len(cls_args) != len(value):
            raise DecodeError(value, cls, "mismatched length")
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
            raise DecodeError(value, cls)
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
            raise DecodeError(value, cls)
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
            reason = f"expected value to be one of {', '.join(_type_name(cls_arg) for cls_arg in cls_args)}"
            raise DecodeError(value, cls, reason)

    # Union
    if get_origin(cls) is Union:
        cls_args = get_args(cls)
        for cls_alt in cls_args:
            try:
                return decode(cls_alt, value)
            except DecodeError:
                continue
        reason = f"expected value to be of type {' | '.join(_type_name(cls_arg) for cls_arg in cls_args)}"
        raise DecodeError(value, cls, reason)

    # Dataclass from {_TAG: "<cls>", _CONTENTS: [...]}
    if is_dataclass(cls):
        if not isinstance(value, Dict):
            raise DecodeError(value, cls, "expected dict")

        # Find the subclass
        if _TYPE not in value:
            raise DecodeError(value, cls, f"missing field '{_TYPE}'")

        subcls_name: str = decode(str, value[_TYPE])
        subcls: Optional[Type[_T]] = _subcls_find(cast(Type[_T], cls), subcls_name)
        if subcls is None:
            raise DecodeError(value, cls, f"could not find class {subcls_name}")
        if not is_dataclass(subcls):
            raise DecodeError(value, subcls, f"not a dataclass")

        # Check if subcls requires any arguments:
        required_init_fields = [
            fld
            for fld in fields(subcls)
            if fld.init and fld.default is MISSING and fld.default_factory is MISSING
        ]
        optional_init_fields = [
            fld
            for fld in fields(subcls)
            if fld.init
            and (fld.default is not MISSING or fld.default_factory is not MISSING)
        ]
        if len(required_init_fields) == 0:
            if _ARGS in value and value[_ARGS]:
                raise DecodeError(value, subcls, f"unused field '{_ARGS}'")
            else:
                return cast(_T, subcls())

        # Decode the arguments:
        if _ARGS not in value:
            raise DecodeError(value, subcls, f"missing field '{_ARGS}'")
        value_args = value[_ARGS]

        # Check if subcls has only a single argument:
        if len(required_init_fields) == 1:
            value_args = [value_args]
        elif not isinstance(value_args, List):
            raise DecodeError(value, subcls, "expected list")

        args: List[Any] = []
        for index, fld in enumerate(required_init_fields):
            if index < len(value_args):
                try:
                    args.append(decode(fld.type, value_args[index]))
                except DecodeError as e:
                    raise DecodeFieldError(value, ((subcls, fld.name),), e)
                except DecodeFieldError as e:
                    raise DecodeFieldError(
                        value, ((subcls, fld.name), *e.telescope), e.reason
                    )
            else:
                raise DecodeError(value, subcls, f"missing value for {fld.name}")

        return cast(_T, subcls(*args))

    raise DecodeError(value, cls)
