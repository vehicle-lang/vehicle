import collections.abc
from abc import ABCMeta, abstractmethod
from dataclasses import MISSING, dataclass, field, fields, is_dataclass
from typing import (
    Any,
    Callable,
    Dict,
    Generic,
    Iterator,
    List,
    Optional,
    Sequence,
    Tuple,
    Type,
    Union,
    cast,
)

from typing_extensions import (
    Literal,
    TypeAlias,
    TypeVar,
    assert_never,
    get_args,
    get_origin,
    override,
)

_S = TypeVar("_S")
_T = TypeVar("_T")


JsonValue: TypeAlias = Union[
    None, str, bool, int, float, complex, List["JsonValue"], Dict[str, "JsonValue"]
]


@dataclass
class DecodeError(Exception):
    value: JsonValue
    cls: Any
    reason: Optional[str] = None
    telescope: Sequence[Tuple[Type[Any], str]] = ()

    def __str__(self) -> str:
        expected_type_str = _type_name(self.cls)
        found_type_str = _type_name(type(self.value))
        telescope_str = " > ".join(
            [f"{_type_name(cls)}.{fld}" for cls, fld in self.telescope]
        )
        value_str = _repr(self.value)
        return "".join(
            [
                f"Could not decode value of type {found_type_str} as {expected_type_str}",
                f": {self.reason}." if self.reason else ".",
                f" ({telescope_str})" if self.telescope else "",
                f"\n{value_str}",
            ]
        )


class Decoder(Generic[_T], metaclass=ABCMeta):
    @abstractmethod
    def decode(
        self,
        decoder: "JsonDecoder",
        cls_origin: Any,
        cls_args: Tuple[Any, ...],
        value: JsonValue,
    ) -> _T:
        ...


class TaggedObjectDecoder(Decoder[_T]):
    TAG: str = "tag"
    CONTENTS: str = "contents"

    @staticmethod
    def _find_class(cls: Type[_T], cls_name: str) -> Optional[Type[_T]]:
        """
        Find a subclass by name.
        """
        for subcls in TaggedObjectDecoder._class_and_subclasses(cls):
            if subcls.__name__ == cls_name:
                return subcls
        return None

    @staticmethod
    def _class_and_subclasses(cls: Type[_T]) -> Iterator[Type[_T]]:
        """
        Iterate over a class and its subclasses.
        """
        yield cls
        yield from cls.__subclasses__()

    @override
    def decode(
        self,
        decoder: "JsonDecoder",
        cls_origin: Type[_T],
        cls_args: Tuple[Any, ...],
        value: JsonValue,
    ) -> _T:
        if not is_dataclass(cls_origin):
            raise DecodeError(value, cls_origin, "not a dataclass")

        if not isinstance(value, Dict):
            raise DecodeError(value, cls_origin, "expected dict")

        # Find the subclass
        if self.TAG not in value:
            raise DecodeError(value, cls_origin, f"missing field '{self.TAG}'")

        subcls_name: str = decoder.decode(str, value[self.TAG])
        subcls: Optional[Type[_T]] = TaggedObjectDecoder._find_class(
            cast(Type[_T], cls_origin), subcls_name
        )
        if subcls is None:
            raise DecodeError(value, cls_origin, f"could not find class {subcls_name}")
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
            if self.CONTENTS in value and value[self.CONTENTS]:
                raise DecodeError(value, subcls, f"unused field '{self.CONTENTS}'")
            else:
                return cast(_T, subcls())

        # Get the arguments field:
        if self.CONTENTS not in value:
            raise DecodeError(value, subcls, f"missing field '{self.CONTENTS}'")
        value_args = value[self.CONTENTS]

        # Special case: If there is only one required argument
        if len(required_init_fields) == 1:
            try:
                fld = required_init_fields[0]
                arg: Any = decoder.decode(fld.type, value_args)
                return cast(_T, subcls(*[arg]))
            except DecodeError as e:
                value_args = [value_args]

        # Decode the arguments:
        if not isinstance(value_args, List):
            raise DecodeError(value, subcls, "expected list")

        args: List[Any] = []

        # Decode required positional arguments:
        for index, fld in enumerate(required_init_fields, start=0):
            if index < len(value_args):
                try:
                    args.append(decoder.decode(fld.type, value_args[index]))
                except DecodeError as e:
                    raise DecodeError(
                        e.value,
                        e.cls,
                        e.reason,
                        telescope=((subcls, fld.name), *e.telescope),
                    )
            else:
                raise DecodeError(value, subcls, f"missing value for {fld.name}")

        # Decode optional positional arguments:
        for index, fld in enumerate(
            optional_init_fields, start=len(required_init_fields)
        ):
            if index < len(value_args):
                try:
                    args.append(decoder.decode(fld.type, value_args[index]))
                except DecodeError as e:
                    raise DecodeError(
                        e.value,
                        e.cls,
                        e.reason,
                        telescope=((subcls, fld.name), *e.telescope),
                    )

        return cast(_T, subcls(*args))


def _decode_None(value: JsonValue) -> None:
    if value is not None:
        raise DecodeError(value, None, "expected None")
    return value


def _decode_str(value: JsonValue) -> str:
    if not isinstance(value, str):
        raise DecodeError(value, str, "expected str")
    return value


def _decode_bool(value: JsonValue) -> bool:
    if not isinstance(value, bool):
        raise DecodeError(value, bool, "expected bool")
    return value


def _decode_int(value: JsonValue) -> int:
    if not isinstance(value, int):
        raise DecodeError(value, int, "expected int")
    return value


def _decode_float(value: JsonValue) -> float:
    if not isinstance(value, (int, float)):
        raise DecodeError(value, float, "expected int or float")
    return value


def _decode_complex(value: JsonValue) -> complex:
    if not isinstance(value, (int, float, complex)):
        raise DecodeError(value, complex, "expected int, float, or complex")
    return value


class TupleDecoder(Decoder[Any]):
    @override
    def decode(
        self,
        decoder: "JsonDecoder",
        cls_origin: Any,
        cls_args: Tuple[Any, ...],
        value: JsonValue,
    ) -> Any:
        if cls_origin is not tuple:
            raise DecodeError(value, cls_origin, "expected tuple")

        if not isinstance(value, List):
            raise DecodeError(value, cls_origin, "expected list")

        # tuple[()]
        if len(cls_args) == 1 and cls_args[0] == ():
            if not value:
                return ()
            else:
                raise DecodeError(value, cls_origin, "mismatched length")

        # tuple[_T, ...]
        if len(cls_args) == 2 and cls_args[1] is ...:
            return tuple(*(decoder.decode(cls_args[0], item) for item in value))

        # tuple[_P]
        if len(cls_args) != len(value):
            raise DecodeError(value, cls_origin, "mismatched length")

        return tuple(
            *(decoder.decode(cls_item, item) for cls_item, item in zip(cls_args, value))
        )


class ListDecoder(Decoder[List[_T]]):
    @override
    def decode(
        self,
        decoder: "JsonDecoder",
        cls_origin: Type[List[_T]],
        cls_args: Tuple[Any, ...],
        value: JsonValue,
    ) -> List[_T]:
        if not isinstance(value, List):
            raise DecodeError(value, cls_origin, "expected list")

        if len(cls_args) <= 0:
            raise DecodeError(value, cls_origin, f"list type requires an argument")

        if len(cls_args) >= 2:
            raise DecodeError(
                value, cls_origin, f"too many arguments: {', '.join(cls_args)}"
            )

        cls_item = cls_args[0]
        return [decoder.decode(cls_item, item) for item in value]


class DictDecoder(Decoder[Dict[_S, _T]]):
    @override
    def decode(
        self,
        decoder: "JsonDecoder",
        cls_origin: Type[Dict[_S, _T]],
        cls_args: Tuple[Any, ...],
        value: JsonValue,
    ) -> Dict[_S, _T]:
        if not isinstance(value, Dict):
            raise DecodeError(value, cls_origin, "expected dict")

        if len(cls_args) <= 1:
            raise DecodeError(value, cls_origin, f"dict type requires arguments")

        if len(cls_args) >= 3:
            raise DecodeError(
                value, cls_origin, f"too many arguments: {', '.join(cls_args)}"
            )

        cls_S = cls_args[0]
        cls_T = cls_args[1]

        return {
            decoder.decode(cls_S, key): decoder.decode(cls_T, val)
            for key, val in value.items()
        }


class LiteralDecoder(Decoder[Any]):
    @override
    def decode(
        self,
        decoder: "JsonDecoder",
        cls_origin: Any,
        cls_args: Tuple[Any, ...],
        value: JsonValue,
    ) -> Any:
        if value in cls_args:
            return value
        else:
            reason = f"expected value to be one of {', '.join(_type_name(cls_arg) for cls_arg in cls_args)}, found {_repr(value)}"
            raise DecodeError(value, cls_origin, reason)


class UnionDecoder(Decoder[Any]):
    @override
    def decode(
        self,
        decoder: "JsonDecoder",
        cls_origin: Any,
        cls_args: Tuple[Any, ...],
        value: JsonValue,
    ) -> Any:
        for cls_arg in cls_args:
            try:
                return decoder.decode(cls_arg, value)
            except DecodeError:
                continue
        reason = f"expected value to be of type {' | '.join(_type_name(cls_arg) for cls_arg in cls_args)}, found {_type_name(value)}"
        raise DecodeError(value, cls_origin, reason)


AnyDecoder: TypeAlias = Union[Decoder[Any], Callable[[JsonValue], Any]]


@dataclass(frozen=True)
class JsonDecoder:
    dataclass_decoder: AnyDecoder
    decoders: Dict[Any, AnyDecoder] = field(init=False, default_factory=dict)

    def register(
        self,
        cls: Union[Type[_T], Any],
        decoder: AnyDecoder,
    ) -> None:
        cls_origin = get_origin(cls) or cls
        self.decoders[cls_origin] = decoder

    def decode(
        self,
        cls: Union[Type[_T], Any],
        value: JsonValue,
    ) -> _T:
        cls_origin = get_origin(cls) or cls
        cls_args = get_args(cls)
        if is_dataclass(cls_origin):
            if isinstance(self.dataclass_decoder, Decoder):
                return cast(
                    _T, self.dataclass_decoder.decode(self, cls_origin, cls_args, value)
                )
            elif callable(self.dataclass_decoder):
                return cast(_T, self.dataclass_decoder(value))
            assert_never()
        else:
            decoder = self.decoders.get(cls_origin)
            if decoder is None:
                raise DecodeError(value, cls, f"no decoder for {_type_name(cls)}")
            elif isinstance(decoder, Decoder):
                return cast(_T, decoder.decode(self, cls_origin, cls_args, value))
            elif callable(decoder):
                return cast(_T, decoder(value))
            assert_never()


_DEFAULT_DECODER: JsonDecoder = JsonDecoder(dataclass_decoder=TaggedObjectDecoder())
_DEFAULT_DECODER.register(type(None), _decode_None)
_DEFAULT_DECODER.register(str, _decode_str)
_DEFAULT_DECODER.register(bool, _decode_bool)
_DEFAULT_DECODER.register(int, _decode_int)
_DEFAULT_DECODER.register(float, _decode_float)
_DEFAULT_DECODER.register(complex, _decode_complex)
_DEFAULT_DECODER.register(tuple, TupleDecoder())
_DEFAULT_DECODER.register(list, ListDecoder())
_DEFAULT_DECODER.register(collections.abc.Sequence, ListDecoder())
_DEFAULT_DECODER.register(collections.abc.MutableSequence, ListDecoder())
_DEFAULT_DECODER.register(dict, DictDecoder())
_DEFAULT_DECODER.register(collections.abc.Mapping, DictDecoder())
_DEFAULT_DECODER.register(collections.abc.MutableMapping, DictDecoder())
_DEFAULT_DECODER.register(Literal, LiteralDecoder())
_DEFAULT_DECODER.register(Union, UnionDecoder())


def decode(cls: Union[Type[_T], Any], value: JsonValue) -> _T:
    return _DEFAULT_DECODER.decode(cls, value)


def _type_name(cls: Any) -> str:
    if isinstance(cls, type):
        return cls.__name__
    else:
        return str(cls)


def _repr(value: Any) -> str:
    value_str = repr(value)
    return value_str[:100]
